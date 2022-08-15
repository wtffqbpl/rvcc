
#include "CGAsm.h"
#include "ASTBaseNode.h"
#include "ASTContext.h"
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

void CodeGenContext::codegen(const Node &ASTTree) {
  genPrologue();
  for (const Node *N = &ASTTree; N; N = N->getNextNode()) {
    genStmt(*N);
    // if stack is dirty, then report error.
    assert(Depth == 0);
  }
  genEpilogue();
}

void CodeGenContext::genStmt(const Node &Nd) {
  if (Nd.getKind() == Node::NKind::ND_EXPR_STMT) {
    genExpr(Nd.getLHS());
    return;
  }
  logging::error("invalid statement");
}

void CodeGenContext::genPrologue() {
  std::cout << ".globl main" << std::endl;
  std::cout << "main:" << std::endl;
  std::cout << "  addi sp, sp, -8" << std::endl;
  // 此时fp 内容存到 sp
  std::cout << "  sd fp, 0(sp)" << std::endl;

  // write sp into fp;
  std::cout << "  mv fp, sp" << std::endl;

  // 26 chars need 26 * 8 bytes space
  std::cout << "  addi, sp, sp -208" << std::endl;
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

void CodeGenContext::genAddr(const Node &Nd) {
  if (Nd.getKind() == Node::NKind::ND_VAR) {
    const std::string_view &VarName = Nd.getName();
    int Offset = (VarName[0] - 'a' + 1) * 8;
    std::cout << "  addi a0, fp, " << -Offset << std::endl;
    return;
  }

  logging::error("not an lvalue");
}

void CodeGenContext::genExpr(const Node &Nd) {
  // generate each leaf node.
  switch (Nd.getKind()) {
  case Node::NKind::ND_NUM:
    // load number to a0
    std::cout << "  li a0, " << Nd.getVal() << std::endl;
    return;
  case Node::NKind::ND_NEG:
    genExpr(Nd.getLHS());
    // neg a0, a0 是 sub a0, x0 的别名，即 a0 = 0 - a0
    std::cout << "  neg a0, a0" << std::endl;
    return;
  case Node::NKind::ND_VAR:
    // variable.
    // 计算出变量的地址，然后存入a0
    genAddr(Nd);
    // 访问a0地址中存储的数据，存入到a0当中
    std::cout << "  ld a0, 0(a0)" << std::endl;
    return;
  case Node::NKind::ND_ASSIGN:
    // 左部是变量的地址，保存值到地址中
    genAddr(Nd.getLHS());
    push();
    // 右部是右值，为表达式的值
    genExpr(Nd.getRHS());
    pop("a1");
    std::cout << "  sd a0, 0(a1)" << std::endl;
    return;
  default:
    break;
  }

  // **先递归到最右的节点, 这种保证了表达式在计算的时候，从右往左来计算。**
  // recursive right node.
  genExpr(Nd.getRHS());
  // push results to stack.
  push();
  // recursive left node.
  genExpr(Nd.getLHS());
  pop("a1");

  // generate each binary tree nodes.
  switch (Nd.getKind()) {
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
    if (Nd.getKind() == Node::NKind::ND_EQ) {
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
