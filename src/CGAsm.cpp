
#include "CGAsm.h"
#include "ASTBaseNode.h"
#include "ASTContext.h"
#include "BasicObjects.h"
#include "rvcc.h"
#include "tokenize.h"
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
#include <stack>
#include <string>

// 压栈，将结果临时压入栈中备用
// sp 为栈指针，栈反向向下增长，
// 当前栈指针的地址就是sp，将a0的值压入栈
// 不使用寄存器存储的原因是因为需要存储的值的数量
//                   STACK
//            |-----------------|  <------ sp
//            |-----------------|  <------ sp - 8
//            |-----------------|  <------ sp - 16
//            |-----------------|  <------ sp - 24
//            |-----------------|
//            |-----------------|
//

CodeGenContext &CodeGenContext::instance() {
  static CodeGenContext codeGenContext;
  return codeGenContext;
}

void CodeGenContext::push() {
  std::cout << "  addi sp, sp, -8" << std::endl;
  std::cout << "  sd a0, 0(sp)" << std::endl;
  ++Depth;
}

// 弹栈，将sp指向的地址的值，弹出到a1
void CodeGenContext::pop(const std::string &Reg) {
  // pop stack value into specified register.
  std::cout << "  ld " << Reg << ", 0(sp)" << std::endl;
  // restore sp to higher address.
  std::cout << "  addi sp, sp, 8" << std::endl;
  --Depth;
}

static void assignLVarOffsets(Function *Prog) {
  int Offset = 0;

  for (VarObj *Var = Prog->locals(); Var; Var = Var->next()) {
    // 每个变量分配 8 bytes, 即在stack 中存放变量的地址
    Offset += 8;
    Var->setOffset(-Offset);
  }

  // 将 stack 对齐到 16 bytes
  Prog->setStackSize(alignTo(Offset, 16));
}

void CodeGenContext::codegen(Function *Prog) {
  assignLVarOffsets(Prog);
  genPrologue(Prog);
  for (Node *N = Prog->body(); N; N = N->getNext()) {
    assert(isa<ExprStmtNode>(N));
    genStmt(N);
    // if stack is dirty, then report error.
    assert(Depth == 0);
  }
  genEpilogue();
}

void CodeGenContext::genStmt(Node *Nd) {
  return genExpr(dynamic_cast<ExprStmtNode *>(Nd)->getChild());
}

void CodeGenContext::genPrologue(Function *Prog) {
  std::cout << ".globl main" << std::endl;
  std::cout << "main:" << std::endl;
  std::cout << "  addi sp, sp, -8" << std::endl;
  // 此时fp 内容存到 sp
  std::cout << "  sd fp, 0(sp)" << std::endl;

  // write sp into fp;
  std::cout << "  mv fp, sp" << std::endl;

  // 偏移量为实际变量所用的 stack 大小
  std::cout << "  add sp, sp " << static_cast<int>(-Prog->stackSize())
            << std::endl;
}

void CodeGenContext::genEpilogue() {
  // fp need write back to sp
  std::cout << "  mv sp, fp" << std::endl;

  // 之前存在sp 的值写回到fp,
  std::cout << "  ld fp, 0(sp)" << std::endl;
  std::cout << "  addi sp, sp, 8" << std::endl;

  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  ret\n";
}

void CodeGenContext::genAddr(Node *Nd) {
  if (isa<VariableNode>(Nd)) {
    auto *Var = dynamic_cast<VariableNode *>(Nd);
    // 偏移量为相对于 fp 的
    std::cout << "  addi a0, fp " << static_cast<int>(Var->getObj().offset())
              << std::endl;
    return;
  }

  logging::error("not an lvalue");
}

void CodeGenContext::genExpr(Node *Nd) {
  // generate each leaf node.
  switch (Nd->getKind()) {
  case Node::NKind::ND_NUM: {
    auto *Num = dynamic_cast<NumNode *>(Nd);
    // load number to a0
    std::cout << "  li a0, " << Num->getValue() << std::endl;
    return;
  }
  case Node::NKind::ND_NEG: {
    auto *Neg = dynamic_cast<NegNode *>(Nd);
    genExpr(Neg->getLHS());
    // neg a0, a0 是 sub a0, x0 的别名，即 a0 = 0 - a0
    std::cout << "  neg a0, a0" << std::endl;
    return;
  }
  case Node::NKind::ND_VAR: {
    // variable.
    // 计算出变量的地址，然后存入a0
    genAddr(Nd);
    // 访问a0地址中存储的数据，存入到a0当中
    std::cout << "  ld a0, 0(a0)" << std::endl;
    return;
  }
  case Node::NKind::ND_ASSIGN: {
    auto *Assign = dynamic_cast<BinaryNode *>(Nd);
    // 左部是变量的地址，保存值到地址中
    genAddr(Assign->getLHS());
    push();
    // 右部是右值，为表达式的值
    genExpr(Assign->getRHS());
    pop("a1");
    std::cout << "  sd a0, 0(a1)" << std::endl;
    return;
  }
  default:
    break;
  }

  assert(isa<BinaryNode>(Nd));
  auto *BNode = dynamic_cast<BinaryNode *>(Nd);
  // **先递归到最右的节点, 这种保证了表达式在计算的时候，从右往左来计算。**
  // recursive right node.
  genExpr(BNode->getRHS());
  // push results to stack.
  push();
  // recursive left node.
  genExpr(BNode->getLHS());
  pop("a1");

  // generate each binary tree nodes.
  switch (BNode->getKind()) {
  case Node::NKind::ND_ADD: // + a0 = a0 + a1
    std::cout << "  add a0, a0, a1" << std::endl;
    return;
  case Node::NKind::ND_SUB:
    std::cout << "  sub a0, a0, a1" << std::endl;
    return;
  case Node::NKind::ND_MUL:
    std::cout << "  mul a0, a0, a1" << std::endl;
    return;
  case Node::NKind::ND_DIV:
    std::cout << "  div a0, a0, a1" << std::endl;
    return;
  case Node::NKind::ND_EQ:
  case Node::NKind::ND_NE:
    std::cout << "  xor a0, a0, a1" << std::endl;
    if (Nd->getKind() == Node::NKind::ND_EQ) {
      // a0 == a1
      //    a0 = a0 ^ a1, sltiu a0, a0, 1
      //    when result is 0 it will be set to 1, vice visa.
      std::cout << "  snez a0, a0" << std::endl;
    }
    return;
  case Node::NKind::ND_LT:
    std::cout << "  slt a0, a0, a1" << std::endl;
    return;
  case Node::NKind::ND_LE:
    std::cout << "  slt a0, a1, a0" << std::endl;
    std::cout << "  xori a0, a0, 1" << std::endl;
    return;
  default:
    break;
  }
  logging::error("invalid expression");
}