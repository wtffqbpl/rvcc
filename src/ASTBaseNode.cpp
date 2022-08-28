#include "ASTBaseNode.h"
#include "BasicObjects.h"
#include "logs.h"
#include "tokenize.h"
#include <map>
#include <unordered_map>

KeywordNode::KeywordNode(c_cyntax::CKType KeywordType, Node *Body)
    : Node(NKind::ND_KEYROWD, getTypeName(Node::NKind::ND_KEYROWD), nullptr),
      BodyNode_(Body) {
  static const std::unordered_map<c_cyntax::CKType, const std::string_view>
      KeyStrToTypeMap = {
#define C_KEYWORD_INFO(Keyword, Expr, Desc)                                    \
  {c_cyntax::CKType::CK_##Keyword, Expr},
#include "c_syntax_info.def"
      };
  KeywordType_ = KeywordType;
  assert(KeyStrToTypeMap.count(KeywordType_) &&
         "Current map should contains keyword");
  KeywordName_ = KeyStrToTypeMap.at(KeywordType);
};

Node *Node::createKeywordNode(c_cyntax::CKType Kind, Node *N1, Node *N2,
                              Node *N3) {
  Node *Nd = nullptr;
  switch (Kind) {
  case c_cyntax::CKType::CK_IF:
  case c_cyntax::CKType::CK_ELSE:
    Nd = new IfCondNode{Kind, N1 /* Cond */, N2 /* Body */, N3 /* ElseN */ };
    break;
  case c_cyntax::CKType::CK_RETURN:
    Nd = new KeywordNode{Kind, N1};
    break;
  default:
    logging::unreachable("cannot handle this keyword type: ", static_cast<unsigned>(Kind));
    break;    
  }
  return Nd;
}

// 解析一元运算符
//    unary = ("+" | "-") unary | primary
Node *Node::createUnaryNode(Node::NKind Kind, Node *Nd) {
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
