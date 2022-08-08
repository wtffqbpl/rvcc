#ifndef PRASE_H
#define PRASE_H

#include <iostream>
#include <memory>
#include <stack>
#include <cstdarg>
#include <string>
#include <cassert>

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

#endif