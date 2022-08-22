#include "ASTBaseNode.h"
#include "BasicObjects.h"
#include "logs.h"
#include "tokenize.h"
#include "unordered_map"

std::unordered_map<Node::NKind, std::string> Node::NodeTypeStrMap_ = {
#define NODE_INFO(Type, Expr, Desc) {Node::NKind::ND_##Type, Desc},
#include "node_type.def"
};

std::unordered_map<std::string_view, KeywordNode::KeywordNT>
    KeywordNode::StrKeywordTMap_ = {
#define C_KEYWORD_INFO(Keyword, Expr, Desc)                                    \
  {Expr, KeywordNode::KeywordNT::NK_##Keyword},
#include "c_syntax_info.def"
};

// 解析一元运算符
//    unary = ("+" | "-") unary | primary
Node *Node::createUnaryNode(Node::NKind Kind, Node *Nd, std::string_view Name) {
  Node *CurNd = nullptr;
  switch (Kind) {
  case Node::NKind::ND_NEG:
    // There's only LSH node.
    CurNd = dynamic_cast<Node *>(new NegNode{Node::getTypeName(Kind), Nd});
    break;
  case Node::NKind::ND_EXPR_STMT:
    // There's only Next node.
    CurNd = dynamic_cast<Node *>(new ExprStmtNode{Node::getTypeName(Kind), Nd});
    break;
  case Node::NKind::ND_KEYROWD:
    CurNd = dynamic_cast<Node *>(new KeywordNode{Name, Nd});
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
  return dynamic_cast<Node *>(
      new BinaryNode{Kind, Node::getTypeName(Kind), LHS, RHS});
}

// create a new number node.
Node *Node::createNumNode(int Val) {
  return dynamic_cast<Node *>(
      new NumNode{Node::getTypeName(NKind::ND_NEG), Val});
}

// create a new variable node.
Node *Node::createVarNode(VarObj *Var) {
  return dynamic_cast<Node *>(new VariableNode{Var});
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
  std::cout << "{TYPE, " << getNodeTypeName(Kind_) << "}";
  ++Depth;
  std::cout << std::endl;
}
