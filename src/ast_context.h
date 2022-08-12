#ifndef SRC_AST_CONTEXT_H
#define SRC_AST_CONTEXT_H

#include <string>
#include <string_view>

class Token;

class Node {
public:
  // AST node type.
  enum class NKind {
    ND_ADD,   // +
    ND_SUB,   // -
    ND_MUL,   // *
    ND_DIV,   // /
    ND_NEG,   // - (unary operator)
    ND_EQ,    // ==
    ND_NE,    // !=
    ND_LT,    // <
    ND_LE,    // <=
    ND_ASSIGN, // assign
    ND_EXPR_STMT, // expression node
    ND_VAR, // variable
    ND_NUM,   // integer
  };

public:
  [[nodiscard]] Node *getNextNode() { return Next; }
  [[nodiscard]] Node *getNextNode() const { return Next; }
  void setNextNode(Node *node_) { Next = node_; }

  [[nodiscard]] Node::NKind getKind() const { return Kind; }

  [[nodiscard]] Node &getLHS() const { return *LHS; }
  [[nodiscard]] Node &getRHS() const { return *RHS; }

  [[nodiscard]] std::string_view getName() { return Name; }
  [[nodiscard]] std::string_view getName() const { return Name; }
  void setVarName(std::string_view Name_) { Name = Name_; }

  [[nodiscard]] int getVal() const { return val; }

  void dump(unsigned Depth=0);

public:
  Node() = default;
  explicit Node(Node::NKind Kind_, int val_, Node *LHS_, Node *RHS_)
          : Kind(Kind_), val(val_), LHS(LHS_), RHS(RHS_) {}

  static Node *createUnaryNode(Node::NKind Kind, Node *Nd);
  static Node *createBinaryNode(Node::NKind Kind, Node *LHS, Node *RHS);
  static Node *createNumNode(int Val);
  static Node *createVarNode(std::string_view Var);

private:
  static Node *newNode(Node::NKind Kind, int Val = 0,
                       Node *LHS = nullptr, Node *RHS = nullptr);

private:
  Node::NKind Kind;       // node type.
  Node *Next = nullptr;   // next node, 下一个语句
  Node *LHS = nullptr;    // left-hand side
  Node *RHS = nullptr;    // right-hand side
  std::string_view Name;  // variable name.
  int val = 0;            // ND_NUM value
};

// Local variable
class Obj {
public:
  [[nodiscard]] Obj *next() { return Next; }
  [[nodiscard]] std::string_view name() const { return NamePtr; }
  [[nodiscard]] unsigned offset() const { return Offset; }

private:
  Obj *Next;                // next obj.
  std::string_view NamePtr; // variable name. TODO: Using string_view
  unsigned Offset;          // fp offset.
};

// Function object.
class Function {
public:
  [[nodiscard]] Node *body() { return Body; }
  [[nodiscard]] Obj *locals() const { return Locals; }
  [[nodiscard]] unsigned stackSize() const { return StackSize; }

private:
  Node *Body = nullptr;     // Function body.
  Obj *Locals = nullptr;    // Local variables.
  unsigned StackSize = 0;   // Stack size.
};

// Generate AST
class ASTContext {
public:
  ASTContext() = default;

  static ASTContext &instance();

// BNF:
//    这样来构建，可以保证优先级没有问题, 越往下，优先级越高
//    program = stmt* // 表示程序是由多个statements(语句)来构成的
//    stmt = exprStmt  // 语句是由表达式语句构成 (后续还会由其他语句)
//    exprStmt = expr ";" // 表达式语句是由表达式 + ";" 组成
//    expr = assign
//    assign = equality ("=" assign)?
//    equality = relational ("==" relational | "!=" relational)*
//    relational = add("<" add | "<=" add | ">" add | ">=" add)*
//    add = mul ("+" mul | "-" mul)*
//    mul = primary ("*" primary | "/" primary)
//    unary = ("+" | "-") unary | primary
//    primary = "(" expr ")" | num
  Node *create(Token *Tok);

  Node *createStmt(Token **Rest, Token *Tok);
  Node *createExprStmt(Token **Rest, Token *Tok);
  Node *createExpr(Token **Rest, Token *Tok);
  Node *createAssignExpr(Token **Rest, Token *Tok);
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

#endif  // SRC_AST_CONTEXT_H
