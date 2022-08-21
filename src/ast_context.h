#ifndef SRC_AST_CONTEXT_H
#define SRC_AST_CONTEXT_H

#include "c_syntax.h"
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

  [[nodiscard]] Node *getNext() { return Next_; }
  void setNext(Node *Next) { Next_ = Next; }

  static Node *createUnaryNode(Node::NKind Kind, Node *Nd);
  static Node *createKeywordNode(c_syntax::CKType Kind, Node *N1,
                                 Node *N2 = nullptr, Node *N3 = nullptr,
                                 Node *N4 = nullptr);
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

    // depress warning.
    // control reaches end of non-void function[-Wreturn-type]
    return std::string{""};
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

class UnaryNode : public Node {
public:
  explicit UnaryNode(Node::NKind Kind, Node *Rhs = nullptr)
      : Node(Kind, Node::getTypeName(Kind)), Rhs_(Rhs) {}

  [[nodiscard]] Node *getRhs() const { return Rhs_; }

  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) {
#define UNARY_NODE_INFO(Keyword, Expr, Desc)                                   \
  case Node::NKind::ND_##Keyword:                                              \
    return true;

    switch (N->getKind()) {
#include "node_type.def"
    default:
      break;
    }
    return false;
  }

private:
  Node *Rhs_;
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
  explicit ExprStmtNode(const std::string_view &&Name, Node *Child)
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
  explicit KeywordNode(c_syntax::CKType KeywordType, Node *L);

  [[nodiscard]] std::string_view getKeywordName() const { return KeywordName_; }
  [[nodiscard]] Node *getBody() const { return BodyNode_; }
  [[nodiscard]] c_syntax::CKType getKeywordType() const { return KeywordType_; }
  void setBody(Node *Body) { BodyNode_ = Body; }

  void print(std::ostream &os) const override {
    os << getKeywordName() << " " << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) {
    return N->getKind() == Node::NKind::ND_KEYROWD;
  }

private:
  Node *BodyNode_;
  c_syntax::CKType KeywordType_;
  std::string_view KeywordName_;
};

/*
 * @brief
 *        if (cond) {
 *          // BodyNode
 *        } else {
 *          // ElseNode
 *        }
 *
 *   This if-statement source code will generate following code:
 *      cond code
 *      beqz cond else  //  if condition is false, then goto else block.
 *      body code
 *      jump to end
 *   .L.else
 *      Else code
 *   .L.end
 */
class IfCondNode : public KeywordNode {
public:
  explicit IfCondNode(c_syntax::CKType KeywordType, Node *Cond, Node *Body,
                      Node *ElseN)
      : KeywordNode(KeywordType, Body), CondNode_(Cond), ElseNode_(ElseN) {}

  [[nodiscard]] Node *getCond() const { return CondNode_; }
  [[nodiscard]] Node *getElse() const { return ElseNode_; }

  void setCond(Node *CondNode) { CondNode_ = CondNode; }
  void setElseN(Node *ElseN) { ElseNode_ = ElseN; }

public:
  static bool isa(const KeywordNode *N) {
    switch (N->getKeywordType()) {
    case c_syntax::CKType::CK_IF:
    case c_syntax::CKType::CK_ELSE:
      return true;
    default:
      break;
    }

    return false;
  }

  /*
   * @brief Count if-statement
   */
  static unsigned getCount() {
    static unsigned Count = 1;
    return Count++;
  }

private:
  Node *CondNode_;
  Node *ElseNode_;
};

class ForLoopNode : public KeywordNode {
public:
  // explicit ForLoopNode(c_syntax::CKType KeywordType, Node *Cond, Node *Init,
  // Node *Body, Node *Inc)
  explicit ForLoopNode(c_syntax::CKType KeywordType, Node *Latch, Node *Header,
                       Node *Body, Node *Exiting)
      : KeywordNode(KeywordType, Body), Latch_(Latch), Header_(Header),
        Exiting_(Exiting) {}

  [[nodiscard]] Node *getLatch() const { return Latch_; }
  [[nodiscard]] Node *getHeader() const { return Header_; }
  [[nodiscard]] Node *getExiting() const { return Exiting_; }

public:
  static unsigned getCount() {
    static unsigned Count = 1;
    return Count++;
  }

private:
  Node *Latch_;
  Node *Header_;
  Node *Exiting_;
};

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

// Local variable
class Obj {
public:
  explicit Obj(const std::string_view Name, Obj *Next)
      : Name_(Name), Next_(Next), Offset_(0) {}

  [[nodiscard]] Obj *next() { return Next_; }
  [[nodiscard]] std::string_view name() const { return Name_; }
  [[nodiscard]] int offset() const { return Offset_; }
  void setOffset(int Offset) { Offset_ = Offset; }

private:
  Obj *Next_;                   // next obj.
  const std::string_view Name_; // variable name. TODO: Using string_view
  int Offset_;                  // fp offset.
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
  explicit VariableNode(Obj *ObjOld)
      : Node(Node::NKind::ND_VAR, Node::getTypeName(Node::NKind::ND_VAR)),
        Old_Obj_(ObjOld) {}

  [[nodiscard]] Obj &getObj() const { return *Old_Obj_; }
  [[nodiscard]] std::string_view getName() const { return Old_Obj_->name(); }
  void print(std::ostream &os) const override {
    os << Node::getTypeName(getKind());
  }

public:
  static bool isa(const Node *N) { return N->getKind() == Node::NKind::ND_VAR; }

private:
  VarInfo *Obj_ = nullptr;
  Obj *Old_Obj_ = nullptr;
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
  //    program = "{" compoundStmt // 表示程序是由多个statements(语句)来构成的
  //    compoundStmt = stmt* "}"
  //    stmt = "return" expr ";"
  //           | "if" "(" expr ")" stmt ("else" stmt)?
  //           | "for" "(" exprStmt expr? ";" expr? ")" stmt
  //           | "while" "(" expr ")" stmt
  //           | "{" compoundStmt
  //           | exprStmt
  //    同时还包含return语句 exprStmt = expr? ";" // 表达式语句是由表达式 + ";"
  //    组成 expr = assign assign = equality ("=" assign)? equality = relational
  //    ("==" relational | "!=" relational)* relational = add("<" add | "<=" add
  //    | ">" add | ">=" add)* add = mul ("+" mul | "-" mul)* mul = primary ("*"
  //    primary | "/" primary)
  //    unary = ("+" | "-" "*" | "&") unary | primary
  //    primary = "("
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