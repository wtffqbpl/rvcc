#include "codegen.h"
#include "parse.h"
#include "tokenize.h"
#include "rvcc.h"
#include <iostream>
#include <memory>
#include <stack>
#include <cstdarg>
#include <string>
#include <cassert>


static ASTContext *astContext = nullptr;

ASTContext &ASTContext::instance() {
  if (astContext == nullptr) {
    astContext = new ASTContext();
  }
  return *astContext;
}

Node *ASTContext::create(Token *Tok) {
  Node Head{};
  Node *Cur = &Head;

  // if (Tok->getKind() != Token::TKind::TK_EOF)
  //   error("Extra token.");
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    Cur->setNextNode(createStmt(&Tok, Tok));
    Cur = Cur->getNextNode();
  }

  return Head.getNextNode();
}

// parse statement.
// stmt = exprStmt
Node *ASTContext::createStmt(Token **Rest, Token *Tok) {
  return createExprStmt(Rest, Tok);
}

// exprStmt = expr ";"
Node *ASTContext::createExprStmt(Token **Rest, Token *Tok) {
  Node *Nd =
      Node::createUnaryNode(Node::NKind::ND_EXPR_STMT, createExpr(&Tok, Tok));
  *Rest = skip(Tok, ";");
  return Nd;
}

// BNF:
//    这样来构建，可以保证优先级没有问题, 越往下，优先级越高
//    program = stmt* // 表示程序是由多个statements(语句)来构成的
//    stmt = exprStmt  // 语句是由表达式语句构成 (后续还会由其他语句)
//    exprStmt = expr ";" // 表达式语句是由表达式 + ";" 组成
//    expr = equality  // 相等性判断
//    equality = relational ("==" relational | "!=" relational)*
//    relational = add("<" add | "<=" add | ">" add | ">=" add)*
//    add = mul ("+" mul | "-" mul)*
//    mul = primary ("*" primary | "/" primary)
//    unary = ("+" | "-") unary | primary
//    primary = "(" expr ")" | num

// expr = exprStmt
Node *ASTContext::createExpr(Token **Rest, Token *Tok) {
  return createEqualityExpr(Rest, Tok);
}

// parse equality
Node *ASTContext::createEqualityExpr(Token **Rest, Token *Tok) {
  // relational
  Node *Nd = createRelationalExpr(&Tok, Tok);

  // ("==" relational | "!=" relational)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    // "==" relational
    if (equal(Tok, "==")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_EQ, Nd,
                                  createRelationalExpr(&Tok, Tok->next()));
      continue;
    }

    // "!=" relational
    if (equal(Tok, "!=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_NE, Nd,
                                  createRelationalExpr(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

Node *ASTContext::createRelationalExpr(Token **Rest, Token *Tok) {
  // add
  Node *Nd = createAddExpr(&Tok, Tok);

  // ("<" add | "<=" add | ">" add | ">=" add)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    // "<" add
    if (equal(Tok, "<")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LT, Nd, createAddExpr(&Tok, Tok->next()));
      continue;
    }

    // "<=" add
    if (equal(Tok, "<=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LE, Nd, createAddExpr(&Tok, Tok->next()));
      continue;
    }

    // ">" add
    // X > Y is equivalent to Y < X
    if (equal(Tok, ">")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LT, createAddExpr(&Tok, Tok->next()), Nd);
      continue;
    }

    // ">=" add
    // X >= Y is equivalent to Y <= X
    if (equal(Tok, ">=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LE, createAddExpr(&Tok, Tok->next()), Nd);
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

Node *ASTContext::createAddExpr(Token **Rest, Token *Tok) {
  // mul
  Node *Nd = createMulExpr(&Tok, Tok);

  // ("+" mul | "-" mul)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    if (equal(Tok, "+")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_ADD, Nd, createMulExpr(&Tok, Tok->next()));
      continue;
    }

    // "-" mul
    if (equal(Tok, "-")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_SUB, Nd, createMulExpr(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// parse multiply/division.
//    mul = primary ("*" primary | "/" primary)*
Node *ASTContext::createMulExpr(Token **Rest, Token *Tok) {
  // unary
  Node *Nd = createUnaryExpr(&Tok, Tok);

  // ("*" unary | "/" unary)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    // "*" unary
    if (equal(Tok, "*")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_MUL, Nd, createUnaryExpr(&Tok, Tok->next()));
      continue;
    }

    // "/" unary
    if (equal(Tok, "/")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_DIV, Nd, createUnaryExpr(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// parse unary node.
//    unary = ("+" | "-") unary | primary
Node *ASTContext::createUnaryExpr(Token **Rest, Token *Tok) {
  // "+" unary
  if (equal(Tok, "+"))
    return createUnaryExpr(Rest, Tok->next());

  // "-" unary
  if (equal(Tok, "-"))
    return Node::createUnaryNode(Node::NKind::ND_NEG, createUnaryExpr(Rest, Tok->next()));

  // primary
  return createPrimaryExpr(Rest, Tok);
}

// parse quotes and number.
//    primary = "(" expr ")" | num
Node *ASTContext::createPrimaryExpr(Token **Rest, Token *Tok) {
  // "(" expr ")"
  if (equal(Tok, "(")) {
    Node *Nd = createExpr(&Tok, Tok->next());
    *Rest = skip(Tok, ")");
    return Nd;
  }

  // num
  if (Tok->getKind() == Token::TKind::TK_NUM) {
    Node *Nd = Node::createNumNode(Tok->getVal());
    *Rest = Tok->next();
    return Nd;
  }

  error("Expected an expression");
  return nullptr;
}

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
  for (const Node *N = &ASTTree; N; N = N->getNextNode())
    genStmt(*N);
  genEpilogue();

  // if stack is dirty, then report error.
  assert(Depth == 0);
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
