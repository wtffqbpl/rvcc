#include "astcontext.h"
#include "codegen.h"
#include "rvcc.h"
#include <iostream>

static CodeGenContext *codeGenContext = nullptr;

CodeGenContext &CodeGenContext::instance() {
  if (codeGenContext == nullptr) {
    codeGenContext = new CodeGenContext;
  }

  return *codeGenContext;
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
  genExpr(ASTTree);
  genEpilogue();

  // if stack is dirty, then report error.
  assert(Depth == 0);
}

void CodeGenContext::genPrologue() {
  std::cout << ".globl main" << std::endl;
  std::cout << "main:" << std::endl;
}

void CodeGenContext::genEpilogue() {
  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  ret\n";
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
