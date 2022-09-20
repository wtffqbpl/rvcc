
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
  std::cout << "  # 压栈，将a0的值压入到栈顶" << std::endl;
  std::cout << "  addi sp, sp, -8" << std::endl;
  std::cout << "  sd a0, 0(sp)" << std::endl;
  ++Depth;
}

// 弹栈，将sp指向的地址的值，弹出到a1
void CodeGenContext::pop(const std::string &Reg) {
  // pop stack value into specified register.
  std::cout << "  # 弹栈，将栈顶的值取出" << std::endl;
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
  std::cout << "# 代码主体" << std::endl;
  genStmt(Prog->body());
  // if stack is dirty, then report error.
  assert(Depth == 0 && "must emit all nodes.");
  genEpilogue();
}

void CodeGenContext::genKeywordCode(KeywordNode *KNode) {
  c_syntax::CKType Type = KNode->getKeywordType();
  switch(Type) {
  case c_syntax::CKType::CK_RETURN:
    std::cout << "  # 返回语句" << std::endl;
    genExpr(KNode->getBody());
    std::cout << "  # 跳转到L.return段" << std::endl;
    std::cout << "  j .L.return" << std::endl;
    break;

  case c_syntax::CKType::CK_IF: {
    // 1. generate if statement.
    auto *IfNode = dynamic_cast<IfCondNode *>(KNode);
    // code body counting.
    unsigned C = IfCondNode::getCount();
    std::cout << "  # ==== 分支语句"
              << " ====" << std::endl;
    // 2. generate if condition statement.
    genExpr(IfNode->getCond());

    // 3. check whether result is 0.
    std::cout << "  # 如果a0 = 0, 即条件为假。则跳转到 L.else." << C << "段\n"
              << std::endl;
    std::cout << "  beqz a0, .L.if.else." << C << std::endl;

    // 4. generate if body statement.
    std::cout << "\n  # 条件为真，则执行if 中的语句。" << std::endl;
    genStmt(IfNode->getBody());
    // jump to exiting block.
    std::cout << "  # 执行完body 中的语句，开始跳转到 L.end." << C << std::endl;
    std::cout << "  j .L.if.end." << C << std::endl;

    // 5. generate else body statement.
    std::cout << "\n  # else 段: " << C << std::endl;
    std::cout << ".L.if.else." << C << std::endl;
    if (IfNode->getElse())
      genStmt(IfNode->getElse());

    // 6. end label for if statement.
    std::cout << " \n  # 分支结束 "
              << ".L.if.end." << C << "段标签:" << std::endl;
    std::cout << ".L.if.end." << C << std::endl;
    break;
  }
  case c_syntax::CKType::CK_FOR: {
    ForLoopNode *ForNode = dynamic_cast<ForLoopNode *>(KNode);
    unsigned C = ForNode->getCount();
    std::cout << "\n # =======循环语句=======" << std::endl;
    // generate for Header;
    if (ForNode->getHeader()) {
      std::cout << "\n# Init 语句." << C << std::endl;
      genStmt(ForNode->getHeader());
    }

    // generate for loop tag.
    std::cout << ".L.for.begin." << C << ":" << std::endl;
    std::cout << "\n  # Cond表达式\n";
    if (ForNode->getLatch()) {
      // generate Latch.
      genExpr(ForNode->getLatch());
      // check whether condition result is zero. and then goto end label
      // when condition result is zero.
      std::cout << "\n # 如果a0 == 0， 跳转到 L.for.end." << C << "段.\n";
      std::cout << "  beqz a0, .L.for.end." << C << std::endl;
    }

    // generate for body.
    std::cout << "\n  # 如果条件成立，执行for循环中的语句\n";
    genStmt(ForNode->getBody());

    // generate exiting.
    if (ForNode->getExiting()) {
      std::cout << "\n  # Inc语句." << C << "\n";
      genExpr(ForNode->getExiting());
    }

    std::cout << "\n  # 执行完Inc 语句之后返回到循环开始\n";
    std::cout << "  j .L.for.begin." << C << ":" << std::endl;
    std::cout << "  .L.for.end." << C << std::endl;
    break;
  }
  default:
    logging::error("Other type of keyword cannot be implemented yet.",
                   static_cast<uint8_t>(Type));
    break;
  }
}

void CodeGenContext::genBlockCode(UnaryNode *UN) {
  for (Node *N = UN->getRhs(); N; N = N->getNext())
    genStmt(N);
}

void CodeGenContext::genStmt(Node *Nd) {
  switch (Nd->getKind()) {
  case Node::NKind::ND_BLOCK:
    genBlockCode(dynamic_cast<UnaryNode *>(Nd));
    return;
  case Node::NKind::ND_KEYROWD:
    genKeywordCode(dynamic_cast<KeywordNode *>(Nd));
    return;
  case Node::NKind::ND_EXPR_STMT:
    genExpr(dynamic_cast<UnaryNode *>(Nd)->getRhs());
    return;
  default:
    break;
  }

  logging::error("invalid statement");
}

void CodeGenContext::genPrologue(Function *Prog) {
  std::cout << "# 定义全局main 段.\n";
  std::cout << ".globl main" << std::endl;

  std::cout << "\n # ====== 程序开始 =======\n";
  std::cout << "main:" << std::endl;
  std::cout
      << "  # 将fp 压入栈，fp 属于 \"被调用者保存\"的寄存器，需要恢复原值\n";
  std::cout << "  addi sp, sp, -8" << std::endl;
  // 此时fp 内容存到 sp
  std::cout << "  sd fp, 0(sp)" << std::endl;

  // write sp into fp;
  std::cout << "  # 新的栈空间\n";
  std::cout << "  mv fp, sp" << std::endl;

  // 偏移量为实际变量所用的 stack 大小
  std::cout << "  # 为程序开辟栈空间\n";
  std::cout << "  addi sp, sp, " << static_cast<int>(-Prog->stackSize())
            << std::endl;
}

void CodeGenContext::genEpilogue() {
  // 输出 return 段标签
  std::cout << "\n # ======程序结束======\n";
  std::cout << "#.L.return 段标签\n";
  std::cout << ".L.return:\n";

  // fp need write back to sp
  std::cout << "  # 栈回收\n";
  std::cout << "  mv sp, fp\n";

  // 之前存在sp 的值写回到fp,
  std::cout << "  # 真正恢复先前的栈帧，把fp,与 sp 恢复到之前的样子。\n";
  std::cout << "  ld fp, 0(sp)\n";
  std::cout << "  addi sp, sp, 8\n";

  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  # 返回a0值给系统调用\n";
  std::cout << "  ret\n";
}

void CodeGenContext::genAddr(Node *Nd) {
  switch (Nd->getKind()) {
  case Node::NKind::ND_VAR: {
    auto *Var = dynamic_cast<VariableNode *>(Nd);
    // 偏移量为相对于 fp 的
    std::cout << "  # 获取变量 " << Var->getObj().name() << "的栈地址："
              << Var->getObj().offset() << "%d(fp)" << std::endl;
    std::cout << "  addi a0, fp, " << static_cast<int>(Var->getObj().offset())
              << std::endl;
    return;
  }
  case Node::NKind::ND_DEREF: {
    genExpr(dynamic_cast<UnaryNode *>(Nd)->getRhs());
    return;
  }
  default:
    break;
  }

  logging::error("not an lvalue");
}

void CodeGenContext::genExpr(Node *Nd) {
  // generate each leaf node.
  switch (Nd->getKind()) {
  case Node::NKind::ND_NUM: {
    auto *Num = dynamic_cast<NumNode *>(Nd);
    // load number to a0
    std::cout << "  # 加载立即数" << std::endl;
    std::cout << "  li a0, " << Num->getValue() << std::endl;
    return;
  }
  case Node::NKind::ND_NEG: {
    auto *Neg = dynamic_cast<UnaryNode *>(Nd);
    genExpr(Neg->getRhs());
    // neg a0, a0 是 sub a0, x0 的别名，即 a0 = 0 - a0
    std::cout << "  # a0 取反 \n";
    std::cout << "  neg a0, a0" << std::endl;
    return;
  }
  case Node::NKind::ND_VAR: {
    // variable.
    // 计算出变量的地址，然后存入a0
    genAddr(Nd);
    std::cout << "  #访问a0地址中存储的数据，存入到a0当中\n";
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
    std::cout << "  # 将 a0的值，写入到a1中存放的地址\n";
    std::cout << "  sd a0, 0(a1)" << std::endl;
    return;
  }
  case Node::NKind::ND_DEREF:
    genExpr(dynamic_cast<UnaryNode *>(Nd)->getRhs());
    std::cout << "  # 读取a0 中存放的地址，然后把值再次存入a0\n";
    std::cout << "  ld a0, 0(a0)" << std::endl;
    return;
  case Node::NKind::ND_ADDR:
    genAddr(dynamic_cast<UnaryNode *>(Nd)->getRhs());
    return;
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
    std::cout << "  # 通过异或判断相等或者不相等\n";
    std::cout << "  xor a0, a0, a1" << std::endl;
    if (Nd->getKind() == Node::NKind::ND_EQ) {
      // a0 == a1
      //    a0 = a0 ^ a1, sltiu a0, a0, 1
      //    when result is 0 it will be set to 1, vice visa.
      std::cout << "  snez a0, a0" << std::endl;
    }
    return;
  case Node::NKind::ND_LT:
    std::cout << "  # 判断 a0 < a1\n";
    std::cout << "  slt a0, a0, a1" << std::endl;
    return;
  case Node::NKind::ND_LE:
    std::cout << "  # 判断a0 <= a1, 这里反转两个操作数\n";
    std::cout << "  slt a0, a1, a0" << std::endl;
    std::cout << "  xori a0, a0, 1" << std::endl;
    return;
  default:
    break;
  }
  logging::error("invalid expression");
}
