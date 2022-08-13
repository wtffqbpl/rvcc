#include "ast_context.h"
#include "rvcc.h"
#include "tokens.h"
#include <iostream>
#include <unordered_map>

// 在解析时，全部的变量实例都被累加到这个列表中。
static Obj *Locals = nullptr;

static Obj *findVar(Token *Tok) {
  for (Obj *Var = Locals; Var; Var = Var->next())
    if (Var->name() == Tok->getTokenName())
      return Var;
  return nullptr;
}

// create a new node.
Node *Node::newNode(Node::NKind Kind, int Val, Node *LHS, Node *RHS) {
  Node *Nd = new Node{Kind, Val, LHS, RHS};
  return Nd;
}

Node *Node::createUnaryNode(Node::NKind Kind, Node *Nd) {
  return newNode(Kind, 0, Nd);
}

// create a new binary tree node.
Node *Node::createBinaryNode(Node::NKind Kind, Node *LHS, Node *RHS) {
  return newNode(Kind, 0, LHS, RHS);
}

// create a new number node.
Node *Node::createNumNode(int Val) {
  return newNode(Node::NKind::ND_NUM, Val);
}

Node *Node::createVarNode(Obj *Var) {
  Node *Nd = newNode(NKind::ND_VAR);
  Nd->setVar(Var);
  return Nd;
}

static Obj *newLVar(std::string_view Name) {
  Obj *Var = new Obj{Name, Locals};
  Locals = Var;
  return Var;
}

static std::string &getNodeTypeName(Node::NKind Kind) {
#define NodeTypeName(Kind) {Node::NKind::Kind, #Kind}
  static std::unordered_map<Node::NKind, std::string> NodeTypeToStrMap = {
          NodeTypeName(ND_ADD),     NodeTypeName(ND_SUB),       NodeTypeName(ND_MUL),
          NodeTypeName(ND_DIV),     NodeTypeName(ND_NEG),       NodeTypeName(ND_EQ),
          NodeTypeName(ND_NE),      NodeTypeName(ND_LT),        NodeTypeName(ND_LE),
          NodeTypeName(ND_ASSIGN),  NodeTypeName(ND_EXPR_STMT), NodeTypeName(ND_VAR),
          NodeTypeName(ND_NUM),
  };

  return NodeTypeToStrMap[Kind];
}

void Node::dump(unsigned Depth) {
  // info indent.
  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";

  std::cout << "{TYPE, " << getNodeTypeName(Kind) << "}";
  ++Depth;
  std::cout << std::endl;

  // children
  if (LHS) LHS->dump(Depth);
  if (RHS) RHS->dump(Depth);

  if (Next != nullptr)
    Next->dump(--Depth);
}

ASTContext &ASTContext::instance() {
  static ASTContext astContext;
  return astContext;
}

Function *ASTContext::create(Token *Tok) {
  Node Head{};
  Node *Cur = &Head;

  // stmt* 会有多个statements
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    Cur->setNextNode(createStmt(&Tok, Tok));
    Cur = Cur->getNextNode();
  }

  Function *Prog = new Function;
  Prog->setBody(Head.getNextNode());
  Prog->setLocals(Locals);
}

static bool equal(Token *Tok, std::string Str) {
  // compare LHS and RHS, if S2
  return std::equal(Tok->getLocation(), Tok->getLocation() + Tok->getLength(),
                    Str.begin());
}

// skip specified string
static Token *skip(Token *Tok, std::string Str) {
  if (!equal(Tok, Str))
    logging::error("expect %s", Str.data());
  return Tok->next();
}

// parse statement.
// stmt = exprStmt
Node *ASTContext::createStmt(Token **Rest, Token *Tok) {
  return createExprStmt(Rest, Tok);
}

// exprStmt = expr ";"
Node *ASTContext::createExprStmt(Token **Rest, Token *Tok) {
  Node *Nd = Node::createUnaryNode(Node::NKind::ND_EXPR_STMT,
                                   createExpr(&Tok, Tok));
  *Rest = skip(Tok, ";");
  return Nd;
}

// create assign expressions.
//    assign = equality ("=" assign)?
Node *ASTContext::createAssignExpr(Token **Rest, Token *Tok) {
  // equality
  Node *Nd = createEqualityExpr(&Tok, Tok);

  // 可能存在递归赋值，如： a = b = 1;
  if (equal(Tok, "="))
    Nd = Node::createBinaryNode(Node::NKind::ND_ASSIGN, Nd,
                                createAssignExpr(&Tok, Tok->next()));

  *Rest = Tok;

  return Nd;
}

// parse expression.
//  expr = assign
Node *ASTContext::createExpr(Token **Rest, Token *Tok) {
  return createAssignExpr(Rest, Tok);
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

  // single variable name.
  if (Tok->getKind() == Token::TKind::TK_IDENT) {
    std::string_view IndentName = Tok->getTokenName();
    Obj *Var = findVar(Tok);
    if (!Var)
      Var = newLVar(IndentName);
    *Rest = Tok->next();
    return Node::createVarNode(Var);
  }

  logging::error("Expected an expression");
  return nullptr;
}
