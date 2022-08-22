//
// Created by CL on 2022/8/15.
//

#ifndef RVCC_BASICNODES_H
#define RVCC_BASICNODES_H

#include "ASTBaseNode.h"
#include "string"

// Local variable
class Obj {
	public:
	  Obj(std::string_view Name_, Obj *Next_) : Name(Name_), Next(Next_) {}
	  [[nodiscard]] Obj *next() { return Next; }
	  [[nodiscard]] std::string_view name() const { return Name; }
	  [[nodiscard]] unsigned offset() const { return Offset; }

	private:
	  Obj *Next;                // next obj.
	  std::string_view Name;    // variable name. TODO: Using string_view
	  unsigned Offset;          // fp offset.
};

// Function object.
class Function {
public:
	[[nodiscard]] Node *body() { return Body; }
	void setBody(Node *Body_) { Body = Body_; }
	[[nodiscard]] Obj *locals() const { return Locals; }
	void setLocals(Obj *Locals_) { Locals = Locals_; }
	[[nodiscard]] unsigned stackSize() const { return StackSize; }
private:
	Node *Body = nullptr;     // Function body.
	Obj *Locals = nullptr;    // Local variables.
	unsigned StackSize = 0;   // Stack size.
};

#endif //RVCC_BASICNODES_H