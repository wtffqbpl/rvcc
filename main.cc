#include <iostream>
#include <memory>
#include <stack>
#include <cstdarg>
#include <string>
#include <cassert>

static void error(std::string StrFmt, ...) {
  // define a va_list variable.
  va_list VA;
  const char *Fmt = StrFmt.data();
  // get all arguments after Fmt.
  va_start(VA, Fmt);
  // output va_list variable using vfprintf.
  vfprintf(stderr, Fmt, VA);
  // add line separator.
  fprintf(stderr, "\n");
  // clear VA
  va_end(VA);
  // terminate program.
  std::exit(1);
}

class Token {
public:
  enum class TKind {
    TK_PUNCT, // operator. + -
    TK_NUM,   // number
    TK_EOF,   // end of file
  };

  Token() = default;

  Token(TKind Kind_, Token *Next_, int Val_, std::string::iterator Loc_,
        size_t Len_)
    : Kind(Kind_), Next(Next_), Val(Val_), Loc(Loc_), Len(Len_) {}

  static Token &instance();
  Token *tokenize(std::string &input);

private:
  Token *createToken(Token::TKind kind,
                            std::string::iterator Start,
                            std::string::iterator End);
  std::string::iterator getNumberPos(std::string &input,
                                            std::string::iterator Start);

public:
  [[nodiscard]] TKind getKind() const { return Kind; }
  [[nodiscard]] Token *next() const {return Next; }
  [[nodiscard]] int getVal() const { return Val; }
  [[nodiscard]] std::string::iterator getLocation() const { return Loc; }
  [[nodiscard]] std::string::size_type getLength() const { return Len; }

private:
  TKind Kind;                 // kind
  Token *Next;                // next token
  int Val;                    // value
  std::string::iterator Loc;  // location
  std::string::size_type Len; // length
};

static Token *TokenHdl = nullptr;

Token &Token::instance() {
  if (TokenHdl == nullptr) {
    TokenHdl = new Token();
  }
  return *TokenHdl;
}

Token *Token::createToken(Token::TKind kind,
                          std::string::iterator Start,
                          std::string::iterator End) {
  auto *Tok = new Token{kind, nullptr, 0, Start,
                        static_cast<size_t>(std::distance(Start, End))};
  return Tok;
}

std::string::iterator Token::getNumberPos(std::string &input,
                                          std::string::iterator Start) {
  auto End = input.end();
  for (; Start != End; ++Start) {
    char achar = *Start;
    if (!std::isdigit(achar))
      return Start;
  }
  return Start;
}

static size_t readPunct(std::string &input, std::string::size_type start) {
  if (start + 1 >= input.size()) {
    std::string punctStr = input.substr(start, 2);
    if (punctStr == "==" || punctStr == "!=" || punctStr == "<=" || punctStr == ">=")
      return 2;
  }
  return std::ispunct(input[start]) ? 1 : 0;
}

// EOF parser.
Token *Token::tokenize(std::string &input) {
  Token Head{};
  Token *Cur = &Head;
  auto It = input.begin();

  for (auto End = input.end(); It != End;) {
    char achar = *It;

    // skip spaces
    if (std::isspace(achar)) {
      ++It;
      continue;
    }

    // parse number.
    if (std::isdigit(achar)) {
      Cur->Next = createToken(Token::TKind::TK_NUM, It, It);
      Cur = Cur->Next;

      auto NumEndPos = getNumberPos(input, It);
      Cur->Val = std::stoi(input.substr(std::distance(input.begin(), It),
                                        std::distance(It, NumEndPos)));
      Cur->Len = std::distance(It, NumEndPos);
      It = NumEndPos;
      continue;
    }

    // parse operators.
    int PunctLen = readPunct(input, std::distance(input.begin(), It));
    if (PunctLen) {
      Cur->Next = createToken(Token::TKind::TK_PUNCT, It, It + PunctLen);
      Cur = Cur->Next;
      ++It;
      // move string iterator PunctLen length.
      It += PunctLen;
      continue;
    }

    // unknown character.
    error("invalid token: %c", achar);
  }

  // Add EOF operator.
  Cur->Next = createToken(Token::TKind::TK_EOF, It, It);

  return Head.Next;
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

class Node {
public:
  // AST node type.
  enum class NKind {
    ND_ADD,   // +
    ND_SUB,   // -
    ND_MUL,   // *
    ND_DIV,   // /
    ND_NUM,   // integer
    ND_NEG,   // - (unary operator)
    ND_EQ,    // ==
    ND_NE,    // !=
    ND_LT,    // <
    ND_LE,    // <=
  };

private:
  Node::NKind Kind;   // node type.
  Node *LHS;          // left-hand side
  Node *RHS;          // right-hand side
  int val;            // ND_NUM value

public:
  [[nodiscard]] int getVal() const { return val; }
  [[nodiscard]] Node::NKind getKind() const { return Kind; }
  [[nodiscard]] Node &getLHS() const { return *LHS; }
  [[nodiscard]] Node &getRHS() const { return *RHS; }

public:
  explicit Node(Node::NKind Kind_, int val_, Node *LHS_, Node *RHS_)
          : Kind(Kind_), val(val_), LHS(LHS_), RHS(RHS_) {}

  static Node *createUnaryNode(Node::NKind Kind, Node *Nd);
  static Node *createBinaryNode(Node::NKind Kind, Node *LHS, Node *RHS);
  static Node *createNumNode(int Val);

private:
  static Node *newNode(Node::NKind Kind, int Val = 0,
                       Node *LHS = nullptr, Node *RHS = nullptr);
};

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

// Generate AST
class ASTContext {
public:
  ASTContext() = default;

// BNF:
//    这样来构建，可以保证优先级没有问题, 越往下，优先级越高
//    expr = equality  // 相等性判断
//    equality = relational ("==" relational | "!=" relational)*
//    relational = add("<" add | "<=" add | ">" add | ">=" add)*
//    add = mul ("+" mul | "-" mul)*
//    mul = primary ("*" primary | "/" primary)
//    unary = ("+" | "-") unary | primary
//    primary = "(" expr ")" | num
  static Node *createExpr(Token **Rest, Token *Tok);
  static Node *createEquality(Token **Rest, Token *Tok);
  static Node *createRelational(Token **Rest, Token *Tok);
  static Node *createAdd(Token **Rest, Token *Tok);
  static Node *createMul(Token **Rest, Token *Tok);
  static Node *createUnary(Token **Rest, Token *Tok);
  static Node *createPrimary(Token **Rest, Token *Tok);

private:
  static Token *CurTok;
};

// parse expression.
//  expr = equality
Node *ASTContext::createExpr(Token **Rest, Token *Tok) {
  return createEquality(Rest, Tok);
}

// parse equality
Node *ASTContext::createEquality(Token **Rest, Token *Tok) {
  // relational
  Node *Nd = createRelational(&Tok, Tok);

  // ("==" relational | "!=" relational)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    // "==" relational
    if (equal(Tok, "==")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_EQ, Nd,
                                  createRelational(&Tok, Tok->next()));
      continue;
    }

    // "!=" relational
    if (equal(Tok, "!=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_NE, Nd,
                                  createRelational(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

Node *ASTContext::createRelational(Token **Rest, Token *Tok) {
  // add
  Node *Nd = createAdd(&Tok, Tok);

  // ("<" add | "<=" add | ">" add | ">=" add)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    // "<" add
    if (equal(Tok, "<")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LT, Nd, createAdd(&Tok, Tok->next()));
      continue;
    }

    // "<=" add
    if (equal(Tok, "<=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LE, Nd, createAdd(&Tok, Tok->next()));
      continue;
    }

    // ">" add
    // X > Y is equivalent to Y < X
    if (equal(Tok, ">")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LT, createAdd(&Tok, Tok->next()), Nd);
      continue;
    }

    // ">=" add
    // X >= Y is equivalent to Y <= X
    if (equal(Tok, ">=")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_LE, createAdd(&Tok, Tok->next()), Nd);
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

Node *ASTContext::createAdd(Token **Rest, Token *Tok) {
  // mul
  Node *Nd = createMul(&Tok, Tok);

  // ("+" mul | "-" mul)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    if (equal(Tok, "+")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_ADD, Nd, createMul(&Tok, Tok->next()));
      continue;
    }

    // "-" mul
    if (equal(Tok, "-")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_SUB, Nd, createMul(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// parse multiply/division.
//    mul = primary ("*" primary | "/" primary)*
Node *ASTContext::createMul(Token **Rest, Token *Tok) {
  // unary
  Node *Nd = createUnary(&Tok, Tok);

  // ("*" unary | "/" unary)*
  while (Tok && Tok->getKind() != Token::TKind::TK_EOF) {
    // "*" unary
    if (equal(Tok, "*")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_MUL, Nd, createUnary(&Tok, Tok->next()));
      continue;
    }

    // "/" unary
    if (equal(Tok, "/")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_DIV, Nd, createUnary(&Tok, Tok->next()));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// parse unary node.
//    unary = ("+" | "-") unary | primary
Node *ASTContext::createUnary(Token **Rest, Token *Tok) {
  // "+" unary
  if (equal(Tok, "+"))
    return createUnary(Rest, Tok->next());

  // "-" unary
  if (equal(Tok, "-"))
    return Node::createUnaryNode(Node::NKind::ND_NEG, createUnary(Rest, Tok->next()));

  // primary
  return createPrimary(Rest, Tok);
}

// parse quotes and number.
//    primary = "(" expr ")" | num
Node *ASTContext::createPrimary(Token **Rest, Token *Tok) {
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

void CodeGenContext::codegen() {
  genPrologue();
  genExpr(ASTTreeNode);
  genEpilogue();

  // if stack is dirty, then report error.
  assert(Depth == 0);
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

// testcase:
//    1 != 2

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Invalid arguments.\n";
    std::cerr << "Usage:\n";
    std::cerr << "\t ./exec val\n";
    error("%s: Invalid number of arguments", argv[0]);
  }

  std::string input(argv[1]);
  Token &TKHdl = Token::instance();
  Token *Tok = TKHdl.tokenize(input);
  Node &Node = *ASTContext::createExpr(&Tok, Tok);

  if (Tok->getKind() != Token::TKind::TK_EOF)
    error("Extra token.");

  CodeGenContext CGCxt{Node};
  CGCxt.codegen();

  return 0;
}
