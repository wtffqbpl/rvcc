#include "ASTBaseNode.h"
#include "BasicObjects.h"
#include "logs.h"
#include "tokenize.h"
#include <map>

std::map<const std::string_view, KeywordNode::KeywordNT>
    KeywordNode::KeyStrToTypeMap_ = {
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
    // C++ 自动向上转型，不需要dynamic_cast
    CurNd = new NegNode{Node::getTypeName(Kind), Nd};
    break;
  case Node::NKind::ND_EXPR_STMT:
    // There's only Next node.
    CurNd = new ExprStmtNode{Node::getTypeName(Kind), Nd};
    break;
  case Node::NKind::ND_KEYROWD:
    CurNd = new KeywordNode{Name, Nd};
    break;
  case Node::NKind::ND_BLOCK:
    CurNd = new BlockNode{Nd};
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
  return new BinaryNode{Kind, Node::getTypeName(Kind), LHS, RHS};
}

// create a new number node.
Node *Node::createNumNode(int Val) {
  return new NumNode{Node::getTypeName(NKind::ND_NEG), Val};
}

// create a new variable node.
Node *Node::createVarNode(VarObj *Var) {
  return new VariableNode{Var};
}

static VarObj *Locals = nullptr;
static VarObj *newLVar(std::string_view Name) {
  VarObj *Var = new VarObj{Name, Locals};
  Locals = Var;
  return Var;
}

void Node::dump(unsigned Depth) {
  // info indent.
  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";
  std::cout << "{TYPE, " << Node::getTypeName(Kind_) << "}";
  ++Depth;
  std::cout << std::endl;
}
