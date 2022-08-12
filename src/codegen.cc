#include "ast_context.h"
#include "codegen.h"
#include "rvcc.h"
#include <iostream>


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
  error("invalid statement");
}

void CodeGenContext::genPrologue() {
  std::cout << ".globl main" << std::endl;
  std::cout << "main:" << std::endl;
  std::cout << "  addi sp, sp, -8" << std::endl;
  std::cout << "  sd fp, 0(sp)" << std::endl;

  // 将sp写入fp
  std::cout << "  mv fp, sp" << std::endl;

  // 26个字母 * 8 bytes = 208 bytes, 需要在stack上面预留208 bytes 空间
  std::cout << "  addi sp, sp, -208" << std::endl;
}

void CodeGenContext::genEpilogue() {
  // 将fp的值改写回sp
  std::cout << "  mv sp, fp" << std::endl;

  // 将最早fp保存的值弹栈，恢复fp
  std::cout << "  ld fp, 0(sp)" << std::endl;
  std::cout << "  addi sp, sp, 8" << std::endl;

  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  ret\n";
}

void CodeGenContext::genAddr(const Node &Nd) {
  if (Nd.getKind() == Node::NKind::ND_VAR) {
    const std::string_view VarName = Nd.getName();
    int Offset = (VarName[0] - 'a' + 1) * 8;
    std::cout << "  addi a0, fp, " << -Offset << std::endl;
    return;
  }

  error("not an lvalue");
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
  error("invalid expression");
}
