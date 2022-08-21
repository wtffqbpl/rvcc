#include "ast_context.h"
#include "codegen.h"
#include "my_timer.h"
#include "rvcc.h"
#include "tokens.h"
#include <fstream>
#include <sstream>

struct CompilerOptions {
  bool ReadFromFile = false;
  char *filename = nullptr;
  bool Debug = false; // debug options.
};

static CompilerOptions CompilerOpts;

std::string getSourceCode() {
  if (CompilerOpts.ReadFromFile) {
    logging::info("Reading source code from file: ", CompilerOpts.filename);
    std::stringstream Oss;
    std::ifstream SrcFileHdl(CompilerOpts.filename);
    Oss << SrcFileHdl.rdbuf();
    return Oss.str();
  }

  logging::info("Reading source code from commandline");
  return std::string{CompilerOpts.filename};
}

static inline void inputArgsCheck(int argc, char **argv) {
  if (argc < 2)
    logging::error("\nInvalid arguments.\n", "Usage:\n",
                   "\t./exec source_code\n");

  // default actions.
  for (unsigned i = 1; i < argc;) {
    if (std::string_view(argv[i]) == "--debug")
      CompilerOpts.Debug = true;

    /*
     * --from-file = 1 --- read source code from file.
     * no --from-file  --- read source code from first argument.
     */
    if (std::string_view(argv[i]) == "--from-file") {
      CompilerOpts.ReadFromFile = true;
      CompilerOpts.filename = argv[++i];
    }

    ++i;
  }

  if (!CompilerOpts.ReadFromFile)
    CompilerOpts.filename = argv[1];
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
  Token *Tok;
  {
    // std::string input = getSourceCode();
    std::string input{argv[1]};
    logging::Timer TokenizeTmr{"Tokenize"};
    Tok = TokenContext::instance().tokenize(std::move(input));
  }

  // construct ast tree.
  Function *Prog;
  {
    logging::Timer ASTTmr{"AST Construction"};
    Prog = ASTContext::instance().create(Tok);
  }

  // code generation.
  {
    logging::Timer CodegenTmr{"Code generation"};
    CodeGenContext::instance().codegen(Prog);
  }

  return 0;
}
