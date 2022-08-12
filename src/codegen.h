#ifndef SRC_CODE_GEN_H
#define SRC_CODE_GEN_H

#include <string>

// 压栈，将结果临时压入栈中备用
// sp 为栈指针，栈反向向下增长，
// 当前栈指针的地址就是sp，将a0的值压入栈
// 不使用寄存器存储的原因是因为需要存储的值的数量
//                   STACK
//            |-----------------|  <------ sp
//            |       fp        |          fp = sp - 8
//            |-----------------|  <------ fp
//            |       'a'       |          fp - 8
//            |       'b'       |          fp - 16
//            |      '...'      |
//            |       'z'       |          fp - 208
//            |-----------------|  <------ sp - 8 - 208
//            |    expr calc    |
//            |-----------------|
//

class Node;

class CodeGenContext {
public:
  explicit CodeGenContext() = default;
  void codegen(const Node &ASTTree);

  static CodeGenContext &instance();

private:
  void genStmt(const Node &Nd);
  void genExpr(const Node &Nd);
  void genAddr(const Node &Nd);
  void genPrologue();
  void genEpilogue();
  void push();
  void pop(const std::string &Reg);

private:
  int Depth = 0;
};

#endif  // SRC_CODE_GEN_H
