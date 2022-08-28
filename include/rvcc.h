#ifndef SRC_RVCC_H
#define SRC_RVCC_H

#include "../src/tokenize.h"
#include "logs.h"
#include <cassert>
#include <cstdarg>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <stack>
#include <string>

static inline int alignTo(int N, int Align) {
  // align to Align.
  return (N + Align - 1) / Align * Align;
}

static bool equal(std::string_view Name, std::string Str) {
  // compare LHS and RHS, if S2
  return Name == Str;
}

struct CompileOptions {
  bool ReadFromFile = false;
  char *FileName = nullptr;
  bool Debug = false;
  CompileOptions() = default;
};

static CompileOptions CompilerOpts;

static std::string &&getSourceCode() {
  if (CompilerOpts.ReadFromFile) {
    logging::info("Reading source code from file: ", CompilerOpts.FileName);
    std::stringstream Oss;
    std::ifstream SrcFileHdl(CompilerOpts.FileName);
    Oss << SrcFileHdl.rdbuf();
    return std::move(Oss.str());
  }

  logging::info("Reading source code from command line");
  return std::move(std::string(CompilerOpts.FileName));
}

static inline void inputArgsCheck(int argc, char **argv) {
  if (argc < 2)
    logging::error("Invalid arguments.\n", "Usage:\n",
                   "\t ./exec source_code\n");

  // default actions.
  for (unsigned i = 1; i < argc;) {
    if (std::string_view(argv[i]) == "--debug") {
      CompilerOpts.Debug = true;

      // --from-file = 1 -> read source code from file.
      // no --file-file  -> read source code from first argument.
      if (std::string_view(argv[i]) == "--from-file") {
        CompilerOpts.ReadFromFile = true;
        CompilerOpts.FileName = argv[++i];
      }
    }
    ++i;
  }

  if (!CompilerOpts.ReadFromFile)
    CompilerOpts.FileName = argv[1];
}

static size_t readPunct(std::string &input, std::string::size_type start) {
  if (start + 1 < input.size()) {
    std::string punctStr = input.substr(start, 2);
    if (punctStr == "==" || punctStr == "!=" || punctStr == "<=" ||
        punctStr == ">=")
      return 2;
  }
  return std::ispunct(input[start]) ? 1 : 0;
}
#endif // SRC_R