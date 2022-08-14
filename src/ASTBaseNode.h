#ifndef PRASE_H
#define PRASE_H
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
#include <stack>
#include <string>

class Node {
public:
  // AST node type.
  enum class NKind {
    ND_ADD,       // +
    ND_SUB,       // -
    ND_MUL,       // *
    ND_DIV,       // /
    ND_NUM,       // integer
    ND_NEG,       // - (unary operator)
    ND_EQ,        // ==
    ND_NE,        // !=
    ND_ASSIGN,    // assignment
    ND_LT,        // <
    ND_LE,        // <=
    ND_EXPR_STMT, // expression node
    ND_VAR,       // variable
  };

private:
  Node::NKind Kind; // node type.
  Node *Next = nullptr;
  Node *LHS = nullptr;
  Node *RHS = nullptr;
  std::string Name;
  int val = 0;

public:
  [[nodiscard]] int getVal() const { return val; }
  [[nodiscard]] Node *getNextNode() { return Next; }
  [[nodiscard]] Node *getNextNode() const { return Next; }
  void setNextNode(Node *node_) { Next = node_; }
  [[nodiscard]] Node::NKind getKind() const { return Kind; }

  [[nodiscard]] Node &getLHS() const { return *LHS; }
  [[nodiscard]] Node &getRHS() const { return *RHS; }

  [[nodiscard]] std::string &getName() { return Name; }
  [[nodiscard]] const std::string &getName() const { return Name; }
  void setVarName(std::string Name_) { Name = Name_; }

  [[nodiscard]] int getVal() { return val; }

public:
  Node() = default;
  explicit Node(Node::NKind Kind_, int val_, Node *LHS_, Node *RHS_)
      : Kind(Kind_), val(val_), LHS(LHS_), RHS(RHS_) {}

  static Node *createUnaryNode(Node::NKind Kind, Node *Nd);
  static Node *createBinaryNode(Node::NKind Kind, Node *LHS, Node *RHS);
  static Node *createNumNode(int Val);
  static Node *createVarNode(std::string &Var);

private:
  static Node *newNode(Node::NKind Kind, int Val = 0, Node *LHS = nullptr,
                       Node *RHS = nullptr);
};

#endif