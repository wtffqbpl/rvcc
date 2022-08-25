//
// Created by CL on 2022/8/14.
//

#include "ASTContext.h"
#include "ASTBaseNode.h"
#include "BasicObjects.h"
#include "CGAsm.h"
#include "rvcc.h"
#include "tokenize.h"
#include <iostream>
#include <unordered_map>
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
#include <stack>
#include <string>


// 在解析时，全部的变量实例都被累加到这个列表中。
static VarObj *Locals = nullptr;

static VarObj *findVar(IndentToken *Tok) {
  for (VarObj *Var = Locals; Var; Var = Var->next())
    if (Var->name() == Tok->getName())
      return Var;
  return nullptr;
}

static VarObj *newLVar(std::string_view Name) {
  VarObj *Var = new VarObj{Name, Locals};
  Locals = Var;
  return Var;
}

ASTContext &ASTContext::instance() {
  static ASTContext astContext;
  return astContext;
}

Function *ASTContext::create(Token *Tok) {
  assert((isa<PunctToken>(Tok) &&
          equal(dynamic_cast<PunctToken *>(Tok)->getName(), "{")) &&
         "program should be start {");
  Tok = Tok->next();
  Function *Prog = new Function;
  Prog->setBody(compoundStmt(&Tok, Tok));
  Prog->setLocals(Locals);

  return Prog;
}

Node *ASTContext::compoundStmt(Token **Rest, Token *Tok) {
  Node Head{Node::NKind::ND_EXPR_STMT,
            Node::getTypeName(Node::NKind::ND_EXPR_STMT)};
  Node *Cur = &Head;

  while (Tok != nullptr &&
         (!isa<PunctToken>(Tok) ||
          !equal(dynamic_cast<PunctToken *>(Tok)->getName(), "}"))) {
    Cur->setNext(createStmt(&Tok, Tok));
    Cur = Cur->getNext();
  }

  // 解析完了body的内容
  Node *Nd = Node::createUnaryNode(Node::NKind::ND_BLOCK, Head.getNext());
  *Rest = Tok->next();
  return Nd;
}

// parse statement.
// stmt = return exprStmt
Node *ASTContext::createStmt(Token **Rest, Token *Tok) {
  // ";" -- aka empty statement.
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), ";")) {
    *Rest = Tok->next();
    return new BlockNode{nullptr};
  }

  // "return expr" ";"
  if (isa<KeyWordToken>(Tok)) {
    std::string_view KeywordName = 
        dynamic_cast<KeyWordToken *>(Tok)->getKeyWordName();
    Node *Nd = Node::createUnaryNode(
        Node::NKind::ND_KEYROWD, createExpr(&Tok, Tok->next()), KeywordName);
    
    *Rest = skipPunct(Tok, ";");
    return Nd;
  }

  // "{" compundStmt.
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "{"))
    return compoundStmt(Rest, Tok->next());

  // exprStmt.
  return createExprStmt(Rest, Tok);
}

// exprStmt = expr ";"
Node *ASTContext::createExprStmt(Token **Rest, Token *Tok) {
  Node *Nd =
      Node::createUnaryNode(Node::NKind::ND_EXPR_STMT, createExpr(&Tok, Tok));
  *Rest = skipPunct(Tok, ";");
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
  return createAssignExpr(Rest, Tok);
}

// expr = assignment
Node *ASTContext::createAssignExpr(Token **Rest, Token *Tok) {
  // equality
  Node *Nd = createEqualityExpr(&Tok, Tok);

  // 可能存在递归赋值？ a = b = 1
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "=")) {
    Nd = Node::createBinaryNode(Node::NKind::ND_ASSIGN, Nd,
                                createAssignExpr(&Tok, Tok->next()));
  }
  *Rest = Tok;
  return Nd;
}

// parse
//    assign = relational ("==" relational | "!=" relational)*
Node *ASTContext::createEqualityExpr(Token **Rest, Token *Tok) {
  // relational
  Node *Nd = createRelationalExpr(&Tok, Tok);

  // ("==" relational | "!=" relational)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    if (!isa<PunctToken>(Tok))
      break;

    auto &PunctTok = *dynamic_cast<PunctToken *>(Tok);
    // "==" relational
    if (equal(PunctTok.getName(), "==")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_EQ, Nd,
                                  createRelationalExpr(&Tok, Tok->next()));
      continue;
    }

    // "!=" relational
    if (equal(PunctTok.getName(), "!=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_NE, Nd,
                                  createRelationalExpr(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// 解析比较关系
// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
Node *ASTContext::createRelationalExpr(Token **Rest, Token *Tok) {
  // add
  Node *Nd = createAddExpr(&Tok, Tok);

  // ("<" add | "<=" add | ">" add | ">=" add)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    if (!isa<PunctToken>(Tok))
      break;
    auto &PunctTok = *dynamic_cast<PunctToken *>(Tok);

    // "<" add
    if (equal(PunctTok.getName(), "<")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LT, Nd,
                                  createAddExpr(&Tok, Tok->next()));
      continue;
    }

    // "<=" add
    if (equal(PunctTok.getName(), "<=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LE, Nd,
                                  createAddExpr(&Tok, Tok->next()));
      continue;
    }

    // ">" add
    // X > Y is equivalent to Y < X
    if (equal(PunctTok.getName(), ">")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LT,
                                  createAddExpr(&Tok, Tok->next()), Nd);
      continue;
    }

    // ">=" add
    // X >= Y is equivalent to Y <= X
    if (equal(PunctTok.getName(), ">=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LE,
                                  createAddExpr(&Tok, Tok->next()), Nd);
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
    if (!isa<PunctToken>(Tok))
      break;

    auto &PunctTok = *dynamic_cast<PunctToken *>(Tok);

    if (equal(PunctTok.getName(), "+")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_ADD, Nd,
                                  createMulExpr(&Tok, Tok->next()));
      continue;
    }

    // "-" mul
    if (equal(PunctTok.getName(), "-")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_SUB, Nd,
                                  createMulExpr(&Tok, Tok->next()));
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
    if (!isa<PunctToken>(Tok))
      break;

    auto &PunctTok = *dynamic_cast<PunctToken *>(Tok);
    // "*" unary
    if (equal(PunctTok.getName(), "*")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_MUL, Nd,
                                  createUnaryExpr(&Tok, Tok->next()));
      continue;
    }

    // "/" unary
    if (equal(PunctTok.getName(), "/")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_DIV, Nd,
                                  createUnaryExpr(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// parse unary node.
// unary = ("+" | "-") unary | primary
Node *ASTContext::createUnaryExpr(Token **Rest, Token *Tok) {
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "+"))
    return createUnaryExpr(Rest, Tok->next());

  // "+" unary
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "+"))
    return createUnaryExpr(Rest, Tok->next());

  // "-" unary
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "-"))
    return Node::createUnaryNode(Node::NKind::ND_NEG,
                                 createUnaryExpr(Rest, Tok->next()));

  // primary
  return createPrimaryExpr(Rest, Tok);
}

// parse quotes and number.
//    primary = "(" expr ")" | num
Node *ASTContext::createPrimaryExpr(Token **Rest, Token *Tok) {
  // "(" expr ")"
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "(")) {
    Node *Nd = createExpr(&Tok, Tok->next());
    *Rest = skipPunct(Tok, ")");
    return Nd;
  }

  // num
  if (isa<NumToken>(Tok)) {
    Node *Nd = Node::createNumNode(dynamic_cast<NumToken *>(Tok)->getVal());
    *Rest = Tok->next();
    return Nd;
  }

  // single char variable name.
  if (isa<IndentToken>(Tok)) {
    auto *IdentTok = dynamic_cast<IndentToken *>(Tok);
    std::string_view VarName = IdentTok->getName();
    VarObj *Var = findVar(IdentTok);
    if (!Var)
      Var = newLVar(VarName);
    *Rest = IdentTok->next();
    return Node::createVarNode(Var);
  }

  logging::error("Expected an expression");
  return nullptr;
}
