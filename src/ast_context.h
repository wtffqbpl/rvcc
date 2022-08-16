#ifndef SRC_AST_CONTEXT_H
#define SRC_AST_CONTEXT_H

#include "rvcc.h"
#include <list>
#include <map>
#include <string>
#include <string_view>

class Token;
class Obj;
class Function;

class Node {
public:
  // AST node type.
  enum class NKind {
#define NODE_INFO(Type, Expr, Desc) ND_##Type,
#include "node_type.def"
  };

public:
  [[nodiscard]] Node::NKind getKind() const { return Kind_; };

  void dump(unsigned Depth=0);

  virtual void print(std::ostream &os) const {}

public:
  Node() = delete;
  explicit Node(Node::NKind Kind, const std::string_view &Name,
                Node *Next = nullptr)
      : Kind_(Kind), Name_(Name), Next_(Next) {}

  [[nodiscard]] std::string_view getName() { return Name_; }
  [[nodiscard]] std::string_view getName() const { return Name_; }
  [[nodiscard]] Node *getNext() { return Next_; }
  [[nodiscard]] Node *getNext() const { return Next_; }
  void setNext(Node *Next) { Next_ = Next; }

  static Node *createUnaryNode(Node::NKind Kind, Node *Nd,
                               std::string_view = "");
  static Node *createBinaryNode(Node::NKind Kind, Node *LHS, Node *RHS);
  static Node *createNumNode(int Val);
  static Node *createVarNode(Obj *Var);
  static std::string getTypeName(Node::NKind Kind) {
#define NODE_INFO(Type, Expr, Desc)                                            \
  case Node::NKind::ND_##Type:                                                 \
    return Expr;

    switch (Kind) {
#include "node_type.def"
    default:
      logging::unreachable("unknown node type:", static_cast<uint8_t>(Kind));
      break;
    }
  }

  friend class ASTContext;

protected:
  Node *Next_ = nullptr;

private:
  Node::NKind Kind_;             // node type.
  const std::string_view &Name_; // variable name.
};

class BinaryNode : public Node {
public:
  explicit BinaryNode(Node::NKind Kind, const std::string_view &Name, Node *LHS,
                      Node *RHS)
      : Node(Kind, Name), LHS_(LHS), RHS_(RHS) {}

  [[nodiscard]] Node *getLHS() { return LHS_; }
  [[nodiscard]] Node *getRHS() { return RHS_; }

  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) {
#define BINARY_NODE_INFO(Type, Expr, Desc)                                     \
  case Node::NKind::ND_##Type:                                                 \
    return true;

    switch (N->getKind()) {
#include "node_type.def"
    default:
      break;
    }
    return false;
  }

private:
  Node *LHS_;
  Node *RHS_;
};

class NegNode : public Node {
public:
  explicit NegNode(const std::string_view &Name, Node *LHS = nullptr)
      : Node(Node::NKind::ND_NEG, Name), LHS_(LHS) {}

  [[nodiscard]] const Node *getLHS() const { return LHS_; }
  [[nodiscard]] Node *getLHS() { return LHS_; }

  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) { return N->getKind() == Node::NKind::ND_NEG; }

private:
  Node *LHS_;
};

class NumNode : public Node {
public:
  explicit NumNode(const std::string_view &Name, int Value)
      : Node(Node::NKind::ND_NUM, Name), Value_(Value) {}

  [[nodiscard]] int getValue() const { return Value_; }

  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) { return N->getKind() == Node::NKind::ND_NUM; }

private:
  int Value_;
};

class ExprStmtNode : public Node {
public:
  explicit ExprStmtNode(const std::string_view &Name, Node *Child)
      : Node(Node::NKind::ND_EXPR_STMT, Name, nullptr), Child_(Child) {}

  [[nodiscard]] Node *getChild() const { return Child_; }
  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) {
    return N->getKind() == Node::NKind::ND_EXPR_STMT;
  }

private:
  Node *Child_;
};

class KeywordNode : public Node {
public:
  enum class KeywordNT : uint8_t {
#define C_KEYWORD_INFO(Keyword, Expr, Desc) NK_##Keyword,
#include "c_syntax_info.def"
  };

public:
  explicit KeywordNode(const std::string_view &KeywordName, Node *L)
      : Node(Node::NKind::ND_KEYROWD,
             Node::getTypeName(Node::NKind::ND_KEYROWD), nullptr),
        LHS_(L), KeywordType_(KeywordNode::getKindByName(KeywordName)),
        KeywordName_(KeywordName) {}

  [[nodiscard]] std::string_view getKeywordName() const { return KeywordName_; }
  [[nodiscard]] Node *getLHS() const { return LHS_; }
  [[nodiscard]] KeywordNT getType() const { return KeywordType_; }

  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) {
    return N->getKind() == Node::NKind::ND_KEYROWD;
  }

private:
  static KeywordNode::KeywordNT getKindByName(const std::string_view &NameStr) {
    return KeyStrToTypeMap_[NameStr];
  }

private:
  static std::map<const std::string_view, KeywordNode::KeywordNT>
      KeyStrToTypeMap_;
  Node *LHS_;
  KeywordNode::KeywordNT KeywordType_;
  const std::string_view &KeywordName_;
};

class Obj; // old variable info class.

class BlockNode : public Node {
public:
  explicit BlockNode(Node *Body)
      : Node(Node::NKind::ND_BLOCK, Node::getTypeName(Node::NKind::ND_BLOCK)),
        Body_(Body) {}

  Node *getBody() { return Body_; }

  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) {
    return N->getKind() == Node::NKind::ND_BLOCK;
  }

private:
  Node *Body_;
};

class VariableNode : public Node {
public:
  /*
   * @brief 由于变量需要在 stack
   * 上面申请空间，而且变量的顺序也需要固定，方便后面去获取，
   *        因此也需要使用链表将变量关联在一起.
   *        TODO: 是否可以将变量之间的链表关系放在外面，通过链表数据结构来保证?
   */
  struct VarInfo {
    VarInfo *Next = nullptr;
    std::string_view Name;
    unsigned FrameIdx = 0;
  };

public:
  [[deprecated]] VariableNode(VarInfo *VI)
      : Node(Node::NKind::ND_VAR, Node::getTypeName(Node::NKind::ND_VAR)),
        Obj_(VI) {}

  explicit VariableNode(Obj *ObjOld)
      : Node(Node::NKind::ND_VAR, Node::getTypeName(Node::NKind::ND_VAR)),
        Old_Obj_(ObjOld) {}

  [[nodiscard]] Obj &getObj() const { return *Old_Obj_; }
  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) { return N->getKind() == Node::NKind::ND_VAR; }

private:
  VarInfo *Obj_ = nullptr;
  Obj *Old_Obj_ = nullptr;
};

// Local variable
class Obj {
public:
  Obj(std::string_view Name_, Obj *Next_) : Name(Name_), Next(Next_) {}
  [[nodiscard]] Obj *next() { return Next; }
  [[nodiscard]] std::string_view name() const { return Name; }
  [[nodiscard]] int offset() const { return Offset; }
  void setOffset(int Offset_) { Offset = Offset_; }

private:
  Obj *Next;                // next obj.
  std::string_view Name;    // variable name. TODO: Using string_view
  int Offset;               // fp offset.
};

// Function object.
class Function {
public:
  [[nodiscard]] Node *body() { return Body; }
  [[nodiscard]] Obj *locals() const { return Locals; }
  [[nodiscard]] unsigned stackSize() const { return StackSize; }

  void setBody(Node *Body_) { Body = Body_; }
  void setLocals(Obj *Locals_) { Locals = Locals_; }
  void setStackSize(int StkSize) { StackSize = StkSize; }

private:
  Node *Body = nullptr;     // Function body.
  Obj *Locals = nullptr;    // Local variables.
  int StackSize = 0;        // Stack size.
  std::list<Node *> Body_list;  // TODO: using bi-list
  std::list<Obj *> Locals_list; // TODO: using this for local variables.
};

//############################### Some Utils. #################################
/*
 * @brief 调用每个类型中的isa 静态函数，来判断传入的实例是否是期待的类型.
 */
template <typename ET> bool isa(const Node *N) { return ET::isa(N); }

// Generate AST
class ASTContext {
public:
  ASTContext() = default;

  static ASTContext &instance();

  // BNF:
  //    这样来构建，可以保证优先级没有问题, 越往下，优先级越高
  //    program = stmt* // 表示程序是由多个statements(语句)来构成的
  //    stmt = "return" expr ";" | exprStmt  // 语句是由表达式语句构成,
  //    同时还包含return语句 exprStmt = expr ";" // 表达式语句是由表达式 + ";"
  //    组成 expr = assign assign = equality ("=" assign)? equality = relational
  //    ("==" relational | "!=" relational)* relational = add("<" add | "<=" add
  //    | ">" add | ">=" add)* add = mul ("+" mul | "-" mul)* mul = primary ("*"
  //    primary | "/" primary) unary = ("+" | "-") unary | primary primary = "("
  //    expr ")" | num
  Function *create(Token *Tok);

private:
  Node *compoundStmt(Token **Rest, Token *Tok);
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
};

#endif // SRC_AST_CONTEXT_H