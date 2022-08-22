#ifndef PRASE_H
#define PRASE_H
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>

class VarObj;
class Function;

class Node {
public:
  // AST node type.
  enum class NKind {
#define NODE_INFO(Type, Expr, Desc) ND_##Type,
#include "node_type.def"
  };

public:
	[[nodiscard]] Node::NKind getKind() const { return Kind_; }

 	void dump(unsigned Depth = 0);

	virtual void print(std::ostream &os) {}

public:
  Node() = delete;	// 不能使用这个ctor
	explicit Node(Node::NKind Kind, std::string_view Name, Node *Next = nullptr)
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
	static Node *createVarNode(VarObj *Var);
	static std::string &getTypeName(Node::NKind Kind) {
		return NodeTypeStrMap_[Kind];
	}

	friend class ASTContext;

protected:
	Node *Next_ = nullptr;

private:
	static std::unordered_map<Node::NKind, std::string> NodeTypeStrMap_;
	Node::NKind Kind_; // node type.
	std::string_view Name_;	// variable name.
	int val = 0;
};

// 类型判断
template <typename ET> bool isa(Node *V) {
	try {
		auto *Nd = dynamic_cast<ET *>(V);
		if (Nd && ET::isa(Nd))
			return true;
	} catch (...) {
		// no need to do anything.
	}
	return false;
}

class BinaryNode : public Node {
public:
	explicit BinaryNode(Node::NKind Kind, std::string_view Name, Node *LHS,
											Node *RHS)
					: Node(Kind, Name), LHS_(LHS), RHS_(RHS) {}

	[[nodiscard]] Node *getLHS() { return LHS_; }
	[[nodiscard]] Node *getRHS() { return RHS_; }

	void print(std::ostream &os) override { os << Node::getTypeName(getKind()); }

public:
	static bool isa(const BinaryNode *V) {
#define BINARY_NODE_INFO(Type, Expr, Desc)                                     \
  case Node::NKind::ND_##Type:                                                 \
    return true;

		switch (V->getKind()) {
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

// 取反
class NegNode : public Node {
public:
	explicit NegNode(std::string_view Name, Node *LHS = nullptr)
					: Node(Node::NKind::ND_NE, Name), LHS_(LHS) {}

	[[nodiscard]] Node *getLHS() const { return LHS_; }
	[[nodiscard]] Node *getLHS() { return LHS_; }

	void print(std::ostream &os) override { os << Node::getTypeName(getKind()); }

public:
	static bool isa(const NegNode *V) {
		return V->getKind() == Node::NKind::ND_NEG;
	}

private:
	Node *LHS_;
};

// 数字
class NumNode : public Node {
public:
	explicit NumNode(std::string_view Name, int Value)
					: Node(Node::NKind::ND_NUM, Name), Value_(Value) {}

	[[nodiscard]] int getValue() const { return Value_; }

	void print(std::ostream &os) override { os << Node::getTypeName(getKind()); }

public:
	static bool isa(const NumNode *V) {
		return V->getKind() == Node::NKind::ND_NUM;
	}

private:
	int Value_;
};

// 表达式
class ExprStmtNode : public Node {
public:
	explicit ExprStmtNode(std::string_view Name, Node *Child)
					: Node(Node::NKind::ND_EXPR_STMT, Name, nullptr), Child_(Child) {}

	[[nodiscard]] Node *getChild() const { return Child_; }
	void print(std::ostream &os) override { os << Node::getTypeName(getKind()); }

public:
	static bool isa(const ExprStmtNode *V) {
		return V->getKind() == Node::NKind::ND_EXPR_STMT;
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
  explicit KeywordNode(std::string_view KeywordName, Node *L)
      : Node(Node::NKind::ND_KEYROWD,
             Node::getTypeName(Node::NKind::ND_KEYROWD), nullptr),
        LHS_(L), KeywordName_(KeywordName) {}

  [[nodiscard]] std::string_view getKeywordName() const { return KeywordName_; }
  [[nodiscard]] Node *getLHS() const { return LHS_; }
  [[nodiscard]] KeywordNT getKeywordType() const {
    return StrKeywordTMap_[KeywordName_];
  }
  void print(std::ostream &os) override { os << Node::getTypeName(getKind()); }

public:
  static bool isa(const KeywordNode *V) {
    return V->getKind() == Node::NKind::ND_KEYROWD;
  }

private:
  static std::unordered_map<std::string_view, KeywordNode::KeywordNT>
      StrKeywordTMap_;
  Node *LHS_;
  std::string_view KeywordName_;
};

class VarObj; // old variable info class.

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
	[[maybe_unused]] VariableNode(VarInfo *VI)
					: Node(Node::NKind::ND_VAR, Node::getTypeName(Node::NKind::ND_VAR)),
						Obj_(VI) {}

	explicit VariableNode(VarObj *ObjOld)
					: Node(Node::NKind::ND_VAR, Node::getTypeName(Node::NKind::ND_VAR)),
						Old_Obj_(ObjOld) {}

	[[nodiscard]] VarObj &getObj() const { return *Old_Obj_; }
	void print(std::ostream &os) override { os << Node::getTypeName(getKind()); }

public:
	static bool isa(const VariableNode *V) {
		return V->getKind() == Node::NKind::ND_VAR;
	}

private:
	VarInfo *Obj_ = nullptr;
	VarObj *Old_Obj_ = nullptr;
};



#endif