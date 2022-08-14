#include "ASTBaseNode.h"

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
Node *Node::createNumNode(int Val) { return newNode(Node::NKind::ND_NUM, Val); }