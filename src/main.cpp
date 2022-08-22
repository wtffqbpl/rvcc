#include "ASTBaseNode.h"
#include "ASTContext.h"
#include "CGAsm.h"
#include "logs.h"
#include "my_timer.h"
#include "rvcc.h"
#include "tokenize.h"
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
#include <stack>
#include <string>

class CompileOptions {
public:
  CompileOptions() = default;
  static CompileOptions &instance();
};

CompileOptions &CompileOptions::instance() {
  static CompileOptions compileOpts{};
  return compileOpts;
}

class Frontend {
  Frontend() = default;
  static Frontend &instance();
};

Frontend &Frontend::instance() {
  static Frontend frontend{};
  return frontend;
}

int main(int argc, char **argv) {
  inputArgsCheck(argc, argv);

  std::string input(argv[1]);

  // generate tokens.
  Token *Tok = nullptr;
  {
    std::string input(argv[1]);
    Timer("Tokenize");
    Tok = TokenContext::instance().tokenize(std::move(input));
    Tok->dump();
  }

  // construct ast tree.
  Function *Prog = nullptr;
  {
    Timer("AST Construction");
    Prog = ASTContext::instance().create(Tok);
    Tok->dump();
  }

  // code generation.
  {
    Timer("Code generation");
    CodeGenContext::instance().codegen(Prog);
  }

  return 0;
}
