#include "ASTBaseNode.h"
#include "BasicObjects.h"
#include "tokenize.h"
#include "unordered_map"

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

// create a new variable node.
Node *Node::createVarNode(std::string_view Var) {
  Node *Nd = newNode(NKind::ND_VAR);
  Nd->setVarName(Var);
  return Nd;
}

static VarObj *Locals = nullptr;
static VarObj *newLVar(std::string_view Name) {
  VarObj *Var = new VarObj{Name, Locals};
  Locals = Var;
  return Var;
}

std::string &getNodeTypeName(Node::NKind Kind) {
#define NodeTypeName(Kind) {Node::NKind::Kind, #Kind}
  std::unordered_map<Node::NKind, std::string> NodeTypeToStrMap = {
      NodeTypeName(ND_ADD),    NodeTypeName(ND_SUB),       NodeTypeName(ND_MUL),
      NodeTypeName(ND_DIV),    NodeTypeName(ND_NEG),       NodeTypeName(ND_EQ),
      NodeTypeName(ND_NE),     NodeTypeName(ND_LT),        NodeTypeName(ND_LE),
      NodeTypeName(ND_ASSIGN), NodeTypeName(ND_EXPR_STMT), NodeTypeName(ND_VAR),
      NodeTypeName(ND_NUM),
  };

  return NodeTypeToStrMap[Kind];
}

void Node::dump(unsigned Depth) {
  // info indent.
  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";
  std::cout << "{TYPE, " << getNodeTypeName(Kind) << "}";
  ++Depth;
  std::cout << std::endl;

  // children
  if (LHS)
    LHS->dump(Depth);
  if (RHS)
    RHS->dump(Depth);

  if (Next != nullptr)
    Next->dump(--Depth);
}
