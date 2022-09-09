#include "ast_context.h"
#include "rvcc.h"

Node::Type *Node::Type::TyInt_ = new Type{};

/*
 * @brief pointer type, and points to Base type.
 */
Node::Type *pointerTo(Node::Type *Base) {
  Node::TypeSystemKind Kind = Node::TypeSystemKind::TY_PTR;
  auto *Ty = new Node::Type{Kind, Base};
  return Ty;
}

/*
 * @brief add Types for all nodes.
 */
void addType(Node *Nd) {
  // return directly when node is empty or node type has been set.
  if (!Nd || Nd->getTy())
    return;

  // TODO: set type for all nodes recursively.

  if (isa<BinaryNode>(Nd)) {
    auto &BNode = *dynamic_cast<BinaryNode *>(Nd);
    switch (BNode.getKind()) {
    case Node::NKind::ND_ADD:
    case Node::NKind::ND_SUB:
    case Node::NKind::ND_MUL:
    case Node::NKind::ND_DIV:
    case Node::NKind::ND_ASSIGN:
      BNode.setLTy(BNode.getRhsTy());
      break;
    case Node::NKind::ND_EQ:
    case Node::NKind::ND_NE:
    case Node::NKind::ND_LT:
    case Node::NKind::ND_LE:
      BNode.setTy(Node::Type::getIntTy());
    default:
      break;
    }
    return;
  }

  if (isa<UnaryNode>(Nd)) {
    auto &UNode = *dynamic_cast<UnaryNode *>(Nd);
    auto &RhsNode = *UNode.getRhs();
    switch (UNode.getKind()) {
    case Node::NKind::ND_ADDR:
      UNode.setTy(RhsNode.getTy());
      break;
    case Node::NKind::ND_DEREF:
      UNode.setTy(RhsNode.isPtrTy() ? RhsNode.getPtrTyBase()
                                    : Node::Type::getIntTy());
      break;
    default:
      break;
    }
    return;
  }
  assert("Cannot handle other type of node.");
}