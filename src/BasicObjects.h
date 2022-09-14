//
// Created by CL on 2022/8/15.
//

#ifndef RVCC_BASICOBJECTS_H
#define RVCC_BASICOBJECTS_H

#include "ASTBaseNode.h"
#include "string"

// Local variable
class VarObj {
public:
  VarObj(std::string_view Name_, VarObj *Next_) : Name(Name_), Next(Next_) {}
  [[nodiscard]] VarObj *next() { return Next; }
  [[nodiscard]] std::string_view name() const { return Name; }
  [[nodiscard]] int offset() const { return Offset; }
  void setOffset(int Offset_) { Offset = Offset_; }

private:
  VarObj *Next;          // next obj.
  std::string_view Name; // variable name. TODO: Using string_view
  int Offset;            // fp offset.
};

// Function object.
class Function {
public:
  [[nodiscard]] Node *body() { return Body; }
  [[nodiscard]] VarObj *locals() const { return Locals; }
  [[nodiscard]] unsigned stackSize() const { return StackSize; }
  void setBody(Node *Body_) { Body = Body_; }
  void setLocals(VarObj *Locals_) { Locals = Locals_; }
  void setStackSize(int StkSize) { StackSize = StkSize; }

private:
  Node *Body = nullptr;            // Function body.
  VarObj *Locals = nullptr;        // Local variables.
  int StackSize = 0;               // Stack size.
  std::list<Node *> Body_list;     // TODO: using bi-list
  std::list<VarObj *> Locals_list; // TODO: using this for local variables.
};

#endif //RVCC_BASICOBJECTS_H
