#include "ast_context.h"
#include "rvcc.h"
#include "tokens.h"

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

ASTContext &ASTContext::instance() {
  static ASTContext astContext;
  return astContext;
}

Node *ASTContext::create(Token *Tok) {
  Node Head{};
  Node *Cur = &Head;

  // stmt* 会有多个statements
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    Cur->setNextNode(createStmt(&Tok, Tok));
    Cur = Cur->getNextNode();
  }

  return Head.getNextNode();
}

static bool equal(Token *Tok, std::string Str) {
  // compare LHS and RHS, if S2
  return std::equal(Tok->getLocation(), Tok->getLocation() + Tok->getLength(),
                    Str.begin());
}

// skip specified string
static Token *skip(Token *Tok, std::string Str) {
  if (!equal(Tok, Str))
    error("expect %s", Str.data());
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

// parse expression.
//  expr = exprStmt
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