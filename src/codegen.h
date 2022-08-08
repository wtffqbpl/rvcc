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


class CodeGenContext {
  int Depth = 0;
  const Node &ASTTreeNode;

public:
  explicit CodeGenContext(const Node &TreeNode_) : ASTTreeNode(TreeNode_) {};
  void codegen();

private:
  void genExpr(const Node &Nd);
  void genPrologue();
  void genEpilogue();
  void push();
  void pop(const std::string &Reg);
};


#endif