#include "ASTBaseNode.h"
#include "ASTContext.h"
#include "CGAsm.h"
#include "rvcc.h"
#include "tokenize.h"
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
#include <stack>
#include <string>

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
