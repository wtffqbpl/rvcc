#include "ast_context.h"
#include "codegen.h"
#include "rvcc.h"
#include "tokens.h"
#include "my_timer.h"
#include <iostream>

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

static inline void inputArgsCheck(int argc, char **argv) {
  if (argc != 2)
    logging::error("Invalid arguments.\n", "Usage:\n", "\t./exec val\n");
}

// testcase:
//    {1; {2;} 3;}
//
//                  [compoundStmt]
//                          |
//        ---------------------------------------
//        |                 |                   |
//  [ND_EXPR_STMT]    [compoundStmt]      [NO_EXPR_STMT]
//        |                 |                   |
//        |                 |                   |
//        [1]         [compoundStmt]           [3]
//                          |
//                    [ND_EXPR_STMT]
//                          |
//                         [2]

int main(int argc, char **argv) {
  inputArgsCheck(argc, argv);

  // generate tokens.
  Token *Tok = nullptr;
  {
    std::string input{argv[1]};
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
