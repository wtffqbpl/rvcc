#include "astcontext.h"
#include "codegen.h"
#include "rvcc.h"
#include "tokens.h"
#include <iostream>
#include <memory>

// testcase:
//    1 != 2

static bool inputArgsCheck(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Invalid arguments." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << "\t ./exec val" << std::endl;
    return false;
  }
  return true;
}

int main(int argc, char **argv) {
  if (!inputArgsCheck(argc, argv))
    error("%s: Invalid arguments", argv[0]);

  std::string input(argv[1]);

  // generate tokens.
  Token *Tok = Token::instance().tokenize(input);

  // construct ast tree.
  Node &Node = *ASTContext::instance().create(Tok);

  // code gen.
  CodeGenContext::instance().codegen(Node);

  return 0;
}
