#include "ast_context.h"
#include "c_syntax.h"
#include "rvcc.h"
#include "tokens.h"
#include <iostream>
#include <unordered_map>

// 在解析时，全部的变量实例都被累加到这个列表中。
static Obj *Locals = nullptr;

KeywordNode::KeywordNode(c_syntax::CKType KeywordType, Node *Body)
    : Node(NKind::ND_KEYROWD, getTypeName(NKind::ND_KEYROWD), nullptr),
      BodyNode_(Body) {
  static const std::unordered_map<c_syntax::CKType, const std::string_view>
      KeyStrToTypeMap = {
#define C_KEYWORD_INFO(Keyword, Expr, Desc)                                    \
  {c_syntax::CKType::CK_##Keyword, Expr},
#include "c_syntax_info.def"
      };

  KeywordType_ = KeywordType;
  assert(KeyStrToTypeMap.count(KeywordType_) &&
         "Current map should contains keyword.");
  KeywordName_ = KeyStrToTypeMap.at(KeywordType_);
}

Node *Node::createKeywordNode(c_syntax::CKType Kind, Node *N1, Node *N2,
                              Node *N3, Node *N4) {
  Node *Nd = nullptr;
  switch (Kind) {
  case c_syntax::CKType::CK_IF:
  case c_syntax::CKType::CK_ELSE:
    Nd = new IfCondNode{Kind, N1 /* Cond */, N2 /* Body */, N3 /* ElseN */};
    break;
  case c_syntax::CKType::CK_FOR:
    Nd = new ForLoopNode{Kind, N1 /* Latch */, N2 /* Header */, N3 /* Body */,
                         N4 /* Exiting */};
    break;
  case c_syntax::CKType::CK_RETURN:
    Nd = new KeywordNode{Kind, N1};
    break;
  default:
    logging::unreachable("cannot handle this keyword type: ",
                         static_cast<unsigned>(Kind));
    break;
  }

  return Nd;
}

// 解析一元运算符
//    unary = ("+" | "-") unary | primary
Node *Node::createUnaryNode(Node::NKind Kind, Node *Nd) {
  Node *CurNd = nullptr;
  switch (Kind) {
  case Node::NKind::ND_NEG:
    // There's only LSH node.
    CurNd = new UnaryNode{Kind, Nd};
    break;
  case Node::NKind::ND_EXPR_STMT:
    // There's only Next node.
    CurNd = new UnaryNode{Kind, Nd};
    break;
  case Node::NKind::ND_BLOCK:
    CurNd = new UnaryNode{Kind, Nd};
    break;
  case Node::NKind::ND_ADDR:
  case Node::NKind::ND_DEREF:
    CurNd = new UnaryNode{Kind, Nd};
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
  return new NumNode{Node::getTypeName(NKind::ND_NUM), Val};
}

Node *Node::createVarNode(Obj *Var) { return new VariableNode{Var}; }

void Node::dump(unsigned Depth) {
  // info indent.
  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";

  std::cout << "{TYPE, " << Node::getTypeName(Kind_) << "}";
  ++Depth;
  std::cout << std::endl;
}

static bool equal(std::string_view Name, std::string_view Str) {
  // compare LHS and RHS, if S2
  return Name == Str;
}

// skip specified string
static Token *skipPunct(Token *Tok, std::string_view Str) {
  if (!isa<PunctToken>(Tok) ||
      dynamic_cast<PunctToken *>(Tok)->getName() != Str)
    logging::error("expect ", Str.data());
  return Tok->next();
}

Function *ASTContext::create(Token *Tok) {
  assert((isa<PunctToken>(Tok) &&
          ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "{")) &&
         "program should be started with \"{\".");

  // skip "{"
  Tok = Tok->next();

  Prog_.setBody(compoundStmt(&Tok, Tok));

  return &Prog_;
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
    addType(Cur);
  }

  // Nd 的 Body存储了 {} 内解析的语句
  Node *Nd = Node::createUnaryNode(Node::NKind::ND_BLOCK, Head.getNext());
  *Rest = Tok->next();
  return Nd;
}

// parse statement.
// stmt = "return" expr ";"
//        | "if" "(" expr ")" stmt ("else" stmt)?
//        | "for" "(" exprStmt expr ? ";" expr? ")" stmt
//        | "while" "(" expr ")" stmt
//        | "{" compoundStmt
//        | exprStmt
Node *ASTContext::createStmt(Token **Rest, Token *Tok) {
  // "return" expr ";"
  if (isa<KeywordToken>(Tok) &&
      (dynamic_cast<KeywordToken *>(Tok)->getKeywordType() ==
       c_syntax::CKType::CK_RETURN)) {
    Node *Nd = Node::createKeywordNode(c_syntax::CKType::CK_RETURN,
                                       createExpr(&Tok, Tok->next()));
    *Rest = skipPunct(Tok, ";");
    return Nd;
  }

  // parse if statement
  // "if" "(" expr ")" stmt ("else" stmt)?
  if (isa<KeywordToken>(Tok) &&
      (dynamic_cast<KeywordToken *>(Tok)->getKeywordType() ==
       c_syntax::CKType::CK_IF)) {
    auto &KT = *dynamic_cast<KeywordToken *>(Tok);
    auto &Nd = *dynamic_cast<IfCondNode *>(
        Node::createKeywordNode(KT.getKeywordType(), nullptr));
    Tok = skipPunct(Tok->next(), "(");
    Nd.setCond(createExpr(&Tok, Tok));
    Tok = skipPunct(Tok, ")");

    // stmt, 符合条件后的语句
    Nd.setBody(createStmt(&Tok, Tok));

    // ("else" stmt)? 不符合条件后的语句
    if (isa<KeywordToken>(Tok) &&
        (dynamic_cast<KeywordToken *>(Tok)->getKeywordType() ==
         c_syntax::CKType::CK_ELSE))
      Nd.setElseN(createStmt(&Tok, Tok->next()));

    *Rest = Tok;
    return &Nd;
  }
  // "for" "(" exprStmt expr? ";" expr? ")" stmt
  if (isa<KeywordToken>(Tok) &&
      (dynamic_cast<KeywordToken *>(Tok)->getKeywordType() ==
       c_syntax::CKType::CK_WHILE)) {
    // exprStmt
    assert(isa<PunctToken>(Tok->next()) && "must be a \"(\" punctuation");
    Tok = skipPunct(Tok->next(), "(");
    // latch
    Node *Latch = createExpr(&Tok, Tok);
    assert(isa<PunctToken>(Tok) && "must be a \")\" punctuation.");
    Tok = skipPunct(Tok, ")");
    // body
    Node *Body = createStmt(Rest, Tok);

    // generate for loop instead of while loop.
    return Node::createKeywordNode(c_syntax::CKType::CK_FOR, Latch, nullptr,
                                   Body, nullptr);
  }

  // "for" "(" exprStmt expr? ";" expr? ")" stmt
  if (isa<KeywordToken>(Tok) &&
      (dynamic_cast<KeywordToken *>(Tok)->getKeywordType() ==
       c_syntax::CKType::CK_FOR)) {
    // exprStmt
    Tok = skipPunct(Tok->next(), "(");
    Node *Header = createExprStmt(&Tok, Tok);

    // expr?
    Node *Latch = nullptr;
    if (!isa<PunctToken>(Tok) ||
        !::equal(dynamic_cast<PunctToken *>(Tok)->getName(), ";"))
      Latch = createExpr(&Tok, Tok);
    // ";"
    Tok = skipPunct(Tok, ";");

    // expr?
    Node *Exiting = nullptr;
    if (!isa<PunctToken>(Tok) ||
        !::equal(dynamic_cast<PunctToken *>(Tok)->getName(), ")"))
      Exiting = createExpr(&Tok, Tok);
    Tok = skipPunct(Tok, ")");

    // stmt
    Node *Body = createStmt(Rest, Tok);

    return Node::createKeywordNode(c_syntax::CKType::CK_FOR, Latch, Header,
                                   Body, Exiting);
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
    return new UnaryNode{Node::NKind::ND_BLOCK, nullptr};
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

static Node *createAdd(Node *Lhs, Node *Rhs) {
  addType(Lhs);
  addType(Rhs);

  // cannot handle ptr + ptr expression.
  assert((Lhs->isIntegerTy() || Rhs->isIntegerTy()) &&
         "Cannot handle (ptr + ptr) expression.");

  // num + num
  if (Lhs->isIntegerTy() && Rhs->isIntegerTy())
    return Node::createBinaryNode(Node::NKind::ND_ADD, Lhs, Rhs);

  // commute num + ptr -> ptr + num
  if (Lhs->isIntegerTy() && Rhs->isPtrTy())
    std::swap(Lhs, Rhs);

  // ptr + num -> (char *)ptr + 8 * num
  // FIXME: should we compute sizeof(decltype(*ptr)) * num ?
  Rhs =
      Node::createBinaryNode(Node::NKind::ND_MUL, Rhs, Node::createNumNode(8));
  return Node::createBinaryNode(Node::NKind::ND_ADD, Lhs, Rhs);
}

static Node *createSub(Node *Lhs, Node *Rhs) {
  addType(Lhs);
  addType(Rhs);

  // num - num
  if (Lhs->isIntegerTy() && Rhs->isIntegerTy())
    return Node::createBinaryNode(Node::NKind::ND_SUB, Lhs, Rhs);

  // ptr - num
  if (Lhs->isPtrTy() && Rhs->isIntegerTy()) {
    Rhs = Node::createBinaryNode(Node::NKind::ND_MUL, Rhs,
                                 Node::createNumNode(8));
    addType(Rhs);
    Node *Nd = Node::createBinaryNode(Node::NKind::ND_SUB, Lhs, Rhs);
    Nd->setTy(Lhs->getTy());
    return Nd;
  }

  // ptr - ptr. return element number.
  if (Lhs->isPtrTy() && Rhs->isPtrTy()) {
    Node *Nd = Node::createBinaryNode(Node::NKind::ND_SUB, Lhs, Rhs);
    Nd->setTy(Node::Type::getIntTy());
    return Node::createBinaryNode(Node::NKind::ND_DIV, Nd,
                                  Node::createNumNode(8));
  }

  assert("Cannot other cases.");
  return nullptr;
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
      Nd = createAdd(Nd, createMulExpr(&Tok, Tok->next()));
      continue;
    }

    // "-" mul
    if (equal(PunctTok.getName(), "-")) {
      Nd = createSub(Nd, createMulExpr(&Tok, Tok->next()));
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
//    unary = ("+" | "-" | "*" | "&") unary | primary
Node *ASTContext::createUnaryExpr(Token **Rest, Token *Tok) {
  // FIXME: using punctuation type for type checking.
  // "+" unary
  if (isa<PunctToken>(Tok) &&
      ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "+"))
    return createUnaryExpr(Rest, Tok->next());

  // "-" unary
  if (isa<PunctToken>(Tok) &&
      ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "-"))
    return Node::createUnaryNode(Node::NKind::ND_NEG,
                                 createUnaryExpr(Rest, Tok->next()));

  if (isa<PunctToken>(Tok) &&
      ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "&"))
    return Node::createUnaryNode(Node::NKind::ND_ADDR,
                                 createUnaryExpr(Rest, Tok->next()));

  if (isa<PunctToken>(Tok) &&
      ::equal(dynamic_cast<PunctToken *>(Tok)->getName(), "*"))
    return Node::createUnaryNode(Node::NKind::ND_DEREF,
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
    auto Var = std::find_if(Prog_.var_begin(), Prog_.var_end(),
                            [&IdentTok](const auto &var) {
                              return var->name() == IdentTok->getName();
                            });
    if (Var == Prog_.var_end()) {
      Prog_.addLocals(new Obj{std::string(VarName)});
      Var = std::prev(Prog_.var_end());
    }
    *Rest = IdentTok->next();
    return Node::createVarNode(*Var);
  }

  logging::error("Expected an expression");

  // FIXME: why compiler warning when remove the following line.
  return nullptr;
}