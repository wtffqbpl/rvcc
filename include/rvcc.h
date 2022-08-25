#ifndef SRC_RVCC_H
#define SRC_RVCC_H

#include "../src/tokenize.h"
#include "logs.h"
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
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

// skip specified string
static Token *skipPunct(Token *Tok, std::string_view Str) {
  if (!isa<PunctToken>(Tok) ||
      dynamic_cast<PunctToken *>(Tok)->getName() != Str)
    logging::error("expect %s", Str.data());
  return Tok->next();
}

static inline void inputArgsCheck(int argc, char **argv) {
  if (argc != 2)
    logging::error("Invalid arguments.\n", "Usage:\n", "\t ./exec val\n");
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