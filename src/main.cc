#include "ast_context.h"
#include "codegen.h"
#include "rvcc.h"
#include "tokens.h"
#include "my_timer.h"
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

  // generate tokens.
  Token *Tok = nullptr;
  {
    std::string input{argv[1]};
    Timer("Tokenize");
    Tok = Token::instance().tokenize(std::move(input));
    Tok->dump();
  }


  // construct ast tree.
  Node *Node = nullptr;
  {
    Timer("AST Construction");
    Node = ASTContext::instance().create(Tok);
    Node->dump();
  }

  // code generation.
  {
    Timer("Code generation");
    CodeGenContext::instance().codegen(*Node);
  }

  return 0;
}
