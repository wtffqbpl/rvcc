#include "codegen.h"
#include "ast_context.h"
#include "c_syntax.h"
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

static void assignLVarOffsets(Function *Prog) {
  int Offset = 0;

  for (Obj *Var = Prog->locals(); Var; Var = Var->next()) {
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

  genStmt(Prog->body());
  // if stack is dirty, then report error.
  assert(Depth == 0 && "must emit all nodes.");

  genEpilogue();
}

void CodeGenContext::genKeywordCode(KeywordNode *KNode) {
  c_syntax::CKType Type = KNode->getKeywordType();
  switch (Type) {
  case c_syntax::CKType::CK_RETURN:
    genExpr(KNode->getBody());
    // 无条件跳转语句，跳转到 .L.return 段
    // j offset 是 jal x0, offset 的别名指令
    std::cout << "  j .L.return\n";
    break;
  case c_syntax::CKType::CK_IF: {
    // generate if statement.
    auto *IfNode = dynamic_cast<IfCondNode *>(KNode);
    // code body counting.
    unsigned C = IfCondNode::getCount();
    // generate if condition statement.
    genExpr(IfNode->getCond());
    // check whether result is 0, jump to else label when result is 0.
    std::cout << "  beqz a0, .L.if.else." << C << ':' << std::endl;
    // generate if body statement.
    genStmt(IfNode->getBody());
    // jump to exiting body.
    std::cout << "  j .L.if.end." << C << ':' << std::endl;
    // generate else code body
    std::cout << "  .L.if.else." << C << ':' << std::endl;
    if (IfNode->getElse())
      genStmt(IfNode->getElse());
    // end label for if statement.
    std::cout << ".L.if.end." << C << std::endl;
    break;
  }
  case c_syntax::CKType::CK_FOR: {
    ForLoopNode *ForNode = dynamic_cast<ForLoopNode *>(KNode);
    // generate for header.
    genStmt(ForNode->getHeader());
    // generate for loop tag.
    unsigned C = ForLoopNode::getCount();
    std::cout << ".L.for.begin." << C << ':' << std::endl;
    if (ForNode->getLatch()) {
      // generate Latch.
      genExpr(ForNode->getLatch());
      // check whether condition result is zero, and then goto end label
      // when condition result is zero.
      std::cout << "  beqz a0, .L.for.end." << C << ':' << std::endl;
    }
    // generate for body.
    genStmt(ForNode->getBody());
    // generate exiting.
    if (ForNode->getExiting())
      genExpr(ForNode->getExiting());
    std::cout << "  j .L.for.begin." << C << ':' << std::endl;
    std::cout << " .L.for.end." << C << ':' << std::endl;
    break;
  }
  default:
    logging::error("Other type of keyword cannot be implemented yet.",
                   static_cast<uint8_t>(Type));
    break;
  }
}

void CodeGenContext::genBlockCode(BlockNode *BN) {
  for (Node *N = BN->getBody(); N; N = N->getNext())
    genStmt(N);
}

void CodeGenContext::genStmt(Node *Nd) {
  switch (Nd->getKind()) {
  case Node::NKind::ND_BLOCK:
    genBlockCode(dynamic_cast<BlockNode *>(Nd));
    return;
  case Node::NKind::ND_KEYROWD:
    genKeywordCode(dynamic_cast<KeywordNode *>(Nd));
    return;
  case Node::NKind::ND_EXPR_STMT:
    genExpr(dynamic_cast<ExprStmtNode *>(Nd)->getChild());
    return;
  default:
    break;
  }

  logging::error("invalid statement");
}

void CodeGenContext::genPrologue(Function *Prog) {
  std::cout << ".globl main" << std::endl;
  std::cout << "main:" << std::endl;
  std::cout << "  addi sp, sp, -8" << std::endl;
  std::cout << "  sd fp, 0(sp)" << std::endl;

  // 将sp写入fp
  std::cout << "  mv fp, sp" << std::endl;

  // 偏移量为实际变量所用的 stack 大小
  std::cout << "  add sp, sp " << static_cast<int>(-Prog->stackSize())
            << std::endl;
}

void CodeGenContext::genEpilogue() {
  // 输出 return 段标签
  std::cout << ".L.return:\n";
  // 将fp的值改写回sp
  std::cout << "  mv sp, fp\n";

  // 将最早fp保存的值弹栈，恢复fp
  std::cout << "  ld fp, 0(sp)\n";
  std::cout << "  addi sp, sp, 8\n";

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
    case Node::NKind::ND_VAR:
      // variable.
      // 计算出变量的地址，然后存入a0
      genAddr(Nd);
      // 访问a0地址中存储的数据，存入到a0当中
      std::cout << "  ld a0, 0(a0)" << std::endl;
      return;
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

  assert(isa<BinaryNode>(Nd) && "This node must be a BinaryNode type.");
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
