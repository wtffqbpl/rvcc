#ifndef CODEGEN_H
#define CODEGEN_H

#include "ASTBaseNode.h"

// 压栈，将结果临时压入栈中备用
// sp 为栈指针，栈反向向下增长，
// 当前栈指针的地址就是sp，将a0的值压入栈
// 不使用寄存器存储的原因是因为需要存储的值的数量
//                   STACK
//            |-----------------|  <------ sp
//            |-----------------|  <------ sp - 8
//            |-----------------|  <------ sp - 16
//            |-----------------|  <------ sp - 24
//            |-----------------|
//            |-----------------|
//

class CodeGenContext {
  int Depth = 0;

public:
  explicit CodeGenContext() = default;
  void codegen(Function *Prog);

  static CodeGenContext &instance();

private:
  void genStmt(Node *Nd);
  void genKeywordCode(KeywordNode *Nd);
  void genExpr(Node *Nd);
  void genAddr(Node *Nd);
  void genPrologue(Function *Prog);
  void genEpilogue();
  void push();
  void pop(const std::string &Reg);
};

#endif