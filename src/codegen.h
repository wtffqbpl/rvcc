#ifndef CODEGEN_H
#define CODEGEN_H

#include "parse.h"

class Token;

// Generate AST
class ASTContext {
public:
  ASTContext() = default;

  static ASTContext &instance();

// BNF:
//    这样来构建，可以保证优先级没有问题, 越往下，优先级越高
//    expr = equality  // 相等性判断
//    equality = relational ("==" relational | "!=" relational)*
//    relational = add("<" add | "<=" add | ">" add | ">=" add)*
//    add = mul ("+" mul | "-" mul)*
//    mul = primary ("*" primary | "/" primary)
//    unary = ("+" | "-") unary | primary
//    primary = "(" expr ")" | num
  Node *create(Token *Tok);
  Node *createImpl(Token **Rest, Token *Tok);

  Node *createStmt(Token **Rest, Token *Tok);
  Node *createExprStmt(Token **Rest, Token *Tok);
  Node *createExpr(Token **Rest, Token *Tok);

  Node *createEqualityExpr(Token **Rest, Token *Tok);
  Node *createRelationalExpr(Token **Rest, Token *Tok);
  Node *createAddExpr(Token **Rest, Token *Tok);
  Node *createMulExpr(Token **Rest, Token *Tok);
  Node *createUnaryExpr(Token **Rest, Token *Tok);
  Node *createPrimaryExpr(Token **Rest, Token *Tok);

  friend class Node;

private:
  Token *CurTok;
};

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

class CodeGenContext {
  int Depth = 0;

public:
  explicit CodeGenContext() = default;
  void codegen(const Node &ASTTree);

  static CodeGenContext &instance();

private:
  void genStmt(const Node &Nd);
  void genExpr(const Node &Nd);
  void genPrologue();
  void genEpilogue();
  void push();
  void pop(const std::string &Reg);
};


#endif