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

static bool equal(Token *Tok, std::string Str) {
  // compare LHS and RHS, if S2
  return std::equal(Tok->getLocation(), Tok->getLocation() + Tok->getLength(),
                    Str.begin());
}

// skip specified string
static Token *skip(Token *Tok, std::string Str) {
  if (!equal(Tok, Str))
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