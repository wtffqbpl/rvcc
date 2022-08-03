#include <iostream>
#include <memory>
#include <stack>
#include <cstdarg>
#include <string>
#include <cassert>


struct Token {
  enum class TKind {
    TK_PUNCT, // operator. + -
    TK_NUM,   // number
    TK_EOF,   // end of file
  };

  TKind Kind;                 // kind
  Token *Next;                // next token
  int Val;                    // value
  std::string::iterator Loc;  // location
  size_t Len;                 // length

  Token() = default;

  Token(TKind Kind_, Token *Next_, int Val_, std::string::iterator Loc_,
        size_t Len_)
    : Kind(Kind_), Next(Next_), Val(Val_), Loc(Loc_), Len(Len_) {}
};

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

static int getNumber(Token &Tok) {
  if (Tok.Kind != Token::TKind::TK_NUM)
    error("expect a number.");
  return Tok.Val;
}

static Token *newToken(Token::TKind Kind,
                       std::string::iterator Start,
                       std::string::iterator End) {
  auto *Tok = new Token{Kind, nullptr, 0, Start,
                        static_cast<size_t>(std::distance(Start, End))};
  return Tok;
}

std::string::iterator getNumberPos(std::string &input, std::string::iterator Start) {
  auto End = input.end();
  for (; Start != End; ++Start) {
    char achar = *Start;
    if (!std::isdigit(achar))
      return Start;
  }
  return Start;
}

// EOF parser.
static Token *tokenize(std::string &input) {
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
      Cur->Next = newToken(Token::TKind::TK_NUM, It, It);
      Cur = Cur->Next;

      auto NumEndPos = getNumberPos(input, It);
      Cur->Val = std::stoi(input.substr(std::distance(input.begin(), It),
                                        std::distance(It, NumEndPos)));
      Cur->Len = std::distance(It, NumEndPos);
      It = NumEndPos;
      continue;
    }

    // parse operators.
    if (std::ispunct(achar)) {
      Cur->Next = newToken(Token::TKind::TK_PUNCT, It, It + 1);
      Cur = Cur->Next;
      ++It;
      continue;
    }

    // unknown character.
    error("invalid token: %c", achar);
  }

  // Add EOF operator.
  Cur->Next = newToken(Token::TKind::TK_EOF, It, It);

  return Head.Next;
}

static bool equal(Token *Tok, std::string Str) {
  // compare LHS and RHS, if S2
  return std::equal(Tok->Loc, Tok->Loc + Tok->Len, Str.begin());
}

// skip specified string
static Token *skip(Token *Tok, std::string Str) {
  if (!equal(Tok, Str))
    error("expect %s", Str.data());
  return Tok->Next;
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

  static Node *createExpr(Token **Rest, Token *Tok);
  static Node *createMul(Token **Rest, Token *Tok);
  static Node *createPrimary(Token **Rest, Token *Tok);

private:
  static Token *CurTok;
};

// BNF:
//    这样来构建，可以保证优先级没有问题, 越往下，优先级越高
//    expr = mul ("+" mul | "-" mul)*
//    mul = primary ("*" primary | "/" primary)
//    primary = "(" expr ")" | num

// parse plus/minus operators.
//  expr = mul("+" mul | "-" mul)*
Node *ASTContext::createExpr(Token **Rest, Token *Tok) {
  // mul
  Node *Nd = createMul(&Tok, Tok);

  // ("+" mul | "-" mul)*
  while (Tok && Tok->Kind != Token::TKind::TK_EOF) {
    // "+" mul
    if (equal(Tok, "+")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_ADD, Nd, createMul(&Tok, Tok->Next));
      continue;
    }

    // "-" mul
    if (equal(Tok, "-")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_SUB, Nd, createMul(&Tok, Tok->Next));
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
  // primary
  Node *Nd = createPrimary(&Tok, Tok);

  // ("*" primary | "/" primary)*
  while (Tok && Tok->Kind != Token::TKind::TK_EOF) {
    // "*" primary
    if (equal(Tok, "*")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_MUL, Nd, createPrimary(&Tok, Tok->Next));
      continue;
    }

    // "/" primary
    if (equal(Tok, "/")) {
      Nd = Node::createBinaryNode(Node::NKind::ND_DIV, Nd, createPrimary(&Tok, Tok->Next));
      continue;
    }

    break;
  }

  *Rest = Tok;
  return Nd;
}

// parse quotes and number.
//    primary = "(" expr ")" | num
Node *ASTContext::createPrimary(Token **Rest, Token *Tok) {
  // "(" expr ")"
  if (equal(Tok, "(")) {
    Node *Nd = createExpr(&Tok, Tok->Next);
    *Rest = skip(Tok, ")");
    return Nd;
  }

  // num
  if (Tok->Kind == Token::TKind::TK_NUM) {
    Node *Nd = Node::createNumNode(Tok->Val);
    *Rest = Tok->Next;
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
  // load number to a0
  if (Nd.getKind() == Node::NKind::ND_NUM) {
    std::cout << "  li a0, " << Nd.getVal() << std::endl;
    return;
  }

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
    default:
      break;
  }
  error("invalid expression");
}

// testcase:
//    -2----3 = -1
//            [-]
//          /    \
//        /       \
//       [-]      [-]
//        |        |
//       [2]      [-]
//                 |
//                [-]
//                 |
//                [3]

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Invalid arguments.\n";
    std::cerr << "Usage:\n";
    std::cerr << "\t ./exec val\n";
    error("%s: Invalid number of arguments", argv[0]);
  }

  std::string input(argv[1]);
  auto *Tok = tokenize(input);
  Node &Node = *ASTContext::createExpr(&Tok, Tok);

  if (Tok->Kind != Token::TKind::TK_EOF)
    error("Extra token.");

  CodeGenContext CGCxt{Node};
  CGCxt.codegen();

  return 0;
}
