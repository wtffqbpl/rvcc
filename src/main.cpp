#include "ASTBaseNode.h"
#include "ASTContext.h"
#include "CGAsm.h"
#include "logs.h"
#include "my_timer.h"
#include "rvcc.h"
#include "tokenize.h"
#include <cassert>
#include <cstdarg>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <stack>
#include <string>

int main(int argc, char **argv) {
  inputArgsCheck(argc, argv);

  // generate tokens.
  Token *Tok;
  {
    // std::string input = getSourceCode();
    std::string input{argv[1]};
    Timer TokenizerTmr("Tokenize");
    Tok = TokenContext::instance().tokenize(std::move(input));
    Tok->dump();
  }

  // construct ast tree.
  Function *Prog;
  {
    Timer ASTTmr("AST Construction");
    Prog = ASTContext::instance().create(Tok);
  }

  // code generation.
  {
    Timer CodeGenTmr("Code generation");
    CodeGenContext::instance().codegen(Prog);
  }

  return 0;
}
