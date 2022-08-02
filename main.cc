#include <iostream>
#include <memory>
#include <stack>
#include <stdarg.h>
#include <string>
#include <vector>

enum class TokenKind {
  TK_PUNCT, // operator. + -
  TK_NUM,   // number
  TK_EOF,   // end of file
};

struct Token {
  TokenKind Kind;             // kind
  Token *Next;                // next token
  int Val;                    // value
  std::string::iterator Loc;  // location
  size_t Len;                 // length
};

static void error(char *Fmt, ...) {
  // define a va_list variable.
  va_list VA;
  // get all arguments after Fmt.
  va_start(VA, Fmt);
  // output va_list variable using vfprintf.
  vfprintf(stderr, Fmt, VA);
  // add line separator.
  fprintf(stderr, "\n");
  // clear VA
  va_end(VA);
  // terminate program.
  std::exit(1);
}

static int getNumber(Token &Tok) {
  if (Tok.Kind != TokenKind::TK_NUM)
    error("expect a number.");
  return Tok.Val;
}

static Token *newToken(TokenKind Kind,
                       std::string::iterator Start,
                       std::string::iterator End) {
  auto *Tok = new Token;
  Tok->Kind = Kind;
  Tok->Loc = Start;
  Tok->Len = std::distance(Start, End);
  return Tok;
}

std::string::iterator getNumberPos(std::string &input, std::string::iterator Start) {
  auto End = input.end();
  for (; Start != End; ++Start) {
    char achar = *Start;
    if (!std::isdigit(achar))
      return Start;
  }
  return Start;
}

// EOF parser.
static Token *tokenize(std::string &input) {
  Token Head{};
  Token *Cur = &Head;
  auto It = input.begin();

  for (auto End = input.end(); It != End;) {
    char achar = *It;

    // skip spaces
    if (std::isspace(achar)) {
      ++It;
      continue;
    }

    // parse number.
    if (std::isdigit(achar)) {
      Cur->Next = newToken(TokenKind::TK_NUM, It, It);
      Cur = Cur->Next;

      auto NumEndPos = getNumberPos(input, It);
      Cur->Val = std::stoi(input.substr(std::distance(input.begin(), It),
                                        std::distance(It, NumEndPos)));
      Cur->Len = std::distance(It, NumEndPos);
      It = NumEndPos;
      continue;
    }

    // parse operators.
    if (achar == '+' || achar == '-') {
      Cur->Next = newToken(TokenKind::TK_PUNCT, It, It + 1);
      Cur = Cur->Next;
      ++It;
      continue;
    }

    // unknown character.
    error("invalid token: %c", achar);
  }

  // Add EOF operator.
  Cur->Next = newToken(TokenKind::TK_EOF, It, It);

  return Head.Next;
}

static bool equal(Token *Tok, std::string Str) {
  // compare LHS and RHS, if S2
  return std::equal(Tok->Loc, Tok->Loc + Tok->Len, Str.begin());
}

// skip specified string
static Token *skip(Token *Tok, std::string Str) {
  if (!equal(Tok, Str))
    error("expect %s", Str.data());
  return Tok->Next;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Invalid arguments.\n";
    std::cerr << "Usage:\n";
    std::cerr << "\t ./exec val\n";
    error("%s: Invalid number of arguments", argv[0]);
  }

  std::string input(argv[1]);
  auto *Tok = tokenize(input);

  // header
  std::cout << ".globl main\n";
  std::cout << "main:\n";
  std::cout << "  li a0, " << getNumber(*Tok) << std::endl;
  Tok = Tok->Next;

  while (Tok->Kind != TokenKind::TK_EOF) {
    if (equal(Tok, "+")) {
      Tok = Tok->Next;
      std::cout << "  addi a0, a0, " << getNumber(*Tok) << std::endl;
      Tok = Tok->Next;
      continue;
    }

    // if no subi instr,
    Tok = skip(Tok, "-");
    std::cout << "  addi a0, a0, -" << getNumber(*Tok) << std::endl;
    Tok = Tok->Next;
  }
  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  ret\n";

  return 0;
}
