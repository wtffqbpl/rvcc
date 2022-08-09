#include "ast_context.h"
#include "codegen.h"
#include "rvcc.h"
#include "tokens.h"
#include <iostream>

static bool inputArgsCheck(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Invalid arguments." << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << "\t ./exec val" << std::endl;
    return false;
  }
  return true;
}

// testcase:
//    1+2;3*4;
//
//    [HEAD] ------> [EXPR_STMT] -------> [EXPR_STMT]
//                         |                   |
//                         |                   |
//                        [+]                 [*]
//                       /   \               /   \
//                      /     \             /     \
//                     [1]    [2]          [3]    [4]

int main(int argc, char **argv) {
  if (!inputArgsCheck(argc, argv))
    error("%s: Invalid arguments", argv[0]);

  std::string input(argv[1]);

  // generate tokens.
  Token *Tok = Token::instance().tokenize(input);

  Tok->dump();

  // construct ast tree.
  Node &Node = *ASTContext::instance().create(Tok);
  Node.dump();

  // code gen.
  CodeGenContext::instance().codegen(Node);

  return 0;
}
