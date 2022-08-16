#include "ast_context.h"
#include "rvcc.h"
#include "tokens.h"
#include <iostream>
#include <map>

// 在解析时，全部的变量实例都被累加到这个列表中。
static Obj *Locals = nullptr;

static Obj *findVar(IndentToken *Tok) {
  for (Obj *Var = Locals; Var; Var = Var->next())
    if (Var->name() == Tok->getName())
      return Var;
  return nullptr;
}

std::map<const std::string_view, KeywordNode::KeywordNT>
    KeywordNode::KeyStrToTypeMap_ = {
#define C_KEYWORD_INFO(Keyword, Expr, Desc)                                    \
  {Expr, KeywordNode::KeywordNT::NK_##Keyword},
#include "c_syntax_info.def"
};

// 解析一元运算符
//    unary = ("+" | "-") unary | primary
Node *Node::createUnaryNode(Node::NKind Kind, Node *Nd,
                            const std::string_view &Name) {
  Node *CurNd = nullptr;
  switch (Kind) {
  case Node::NKind::ND_NEG:
    // There's only LSH node.
    CurNd = new NegNode{Node::getTypeName(Kind), Nd};
    break;
  case Node::NKind::ND_EXPR_STMT:
    // There's only Next node.
    CurNd = new ExprStmtNode{Node::getTypeName(Kind), Nd};
    break;
  case Node::NKind::ND_KEYROWD:
    CurNd = new KeywordNode{Name, Nd};
    break;
  case Node::NKind::ND_BLOCK:
    CurNd = new BlockNode{Nd};
    break;
  default:
    logging::error("Cannot handle this type of node: ",
                   static_cast<unsigned>(Kind));
    break;
  }

  return CurNd;
}

// create a new binary tree node.
Node *Node::createBinaryNode(Node::NKind Kind, Node *LHS, Node *RHS) {
  return new BinaryNode{Kind, Node::getTypeName(Kind), LHS, RHS};
}

// create a new number node.
Node *Node::createNumNode(int Val) {
  return new NumNode{Node::getTypeName(NKind::ND_NEG), Val};
}

Node *Node::createVarNode(Obj *Var) { return new VariableNode{Var}; }

static Obj *newLVar(std::string_view Name) {
  Obj *Var = new Obj{Name, Locals};
  Locals = Var;
  return Var;
}

void Node::dump(unsigned Depth) {
  // info indent.
  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";

  std::cout << "{TYPE, " << Node::getTypeName(Kind_) << "}";
  ++Depth;
  std::cout << std::endl;

#if 0
  // children
  if (LHS) LHS->dump(Depth);
  if (RHS) RHS->dump(Depth);

  if (Next != nullptr)
    Next->dump(--Depth);
#endif
}

ASTContext &ASTContext::instance() {
  static ASTContext astContext;
  return astContext;
}

static bool equal(std::string_view Name, std::string_view Str) {
  // compare LHS and RHS, if S2
  return Name == Str;
}

// skip specified string
static Token *skipPunct(Token *Tok, std::string_view Str) {
  if (!isa<PunctToken>(Tok) ||
      dynamic_cast<PunctToken *>(Tok)->getName() != Str)
    logging::error("expect %s", Str.data());
  return Tok->next();
}

Function *ASTContext::create(Token *Tok) {
  assert((isa<PunctToken>(Tok) &&
          ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "{")) &&
         "program should be started with \"{\".");

  // skip "{"
  Tok = Tok->next();

  auto *Prog = new Function;
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
          !::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "}"))) {
    Cur->setNext(createStmt(&Tok, Tok));
    Cur = Cur->getNext();
  }

  // Nd 的 Body存储了 {} 内解析的语句
  Node *Nd = Node::createUnaryNode(Node::NKind::ND_BLOCK, Head.getNext());
  *Rest = Tok->next();
  return Nd;
}

// parse statement.
// stmt = "return" expr ";" | exprStmt
Node *ASTContext::createStmt(Token **Rest, Token *Tok) {
  // "return" expr ";"
  if (isa<KeywordToken>(Tok)) {
    const std::string_view &KeywordName =
        dynamic_cast<KeywordToken *>(Tok)->getKeywordName();
    Node *Nd = Node::createUnaryNode(
        Node::NKind::ND_KEYROWD, createExpr(&Tok, Tok->next()), KeywordName);

    *Rest = skipPunct(Tok, ";");
    return Nd;
  }

  // "{" compundStmt
  if (isa<PunctToken>(Tok) &&
      ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "{"))
    return compoundStmt(Rest, Tok->next());

  // exprStmt
  return createExprStmt(Rest, Tok);
}

// exprStmt = expr? ";"
Node *ASTContext::createExprStmt(Token **Rest, Token *Tok) {
  // ";" -- aka empty statement
  if (isa<PunctToken>(Tok) &&
      ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), ";")) {
    *Rest = Tok->next();
    return new BlockNode{nullptr};
  }

  // expr ";"
  Node *Nd = Node::createUnaryNode(Node::NKind::ND_EXPR_STMT,
                                   createExpr(&Tok, Tok));
  *Rest = skipPunct(Tok, ";");
  return Nd;
}

// create assign expressions.
//    assign = equality ("=" assign)?
Node *ASTContext::createAssignExpr(Token **Rest, Token *Tok) {
  // equality
  Node *Nd = createEqualityExpr(&Tok, Tok);

  // 可能存在递归赋值，如： a = b = 1;
  if (isa<PunctToken>(Tok) &&
      equal(dynamic_cast<PunctToken *>(Tok)->getName(), "="))
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
//    unary = ("+" | "-") unary | primary
Node *ASTContext::createUnaryExpr(Token **Rest, Token *Tok) {
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

  // single variable name.
  if (isa<IndentToken>(Tok)) {
    auto *IdentTok = dynamic_cast<IndentToken *>(Tok);
    std::string_view VarName = IdentTok->getName();
    Obj *Var = findVar(IdentTok);
    if (!Var)
      Var = newLVar(VarName);
    *Rest = IdentTok->next();
    return Node::createVarNode(Var);
  }

  logging::error("Expected an expression");

  // FIXME: why compiler warning when remove the following line.
  return nullptr;
}