#include "rvcc.h"
#include "tokens.h"
#include <string>

Token &Token::instance() {
  static Token TokenHdl;
  return TokenHdl;
}

Token *Token::createToken(Token::TKind kind,
                          std::string::iterator Start,
                          std::string::iterator End) {
  auto *Tok = new Token{kind, nullptr, 0, Start,
                        static_cast<size_t>(std::distance(Start, End))};
  return Tok;
}

std::string::iterator Token::getNumberPos(std::string &input,
                                          std::string::iterator Start) {
  for (auto End = input.end(); Start != End; ++Start) {
    char achar = *Start;
    if (!std::isdigit(achar))
      return Start;
  }
  return Start;
}

static size_t readPunct(std::string &input, std::string::size_type start) {
  if (start + 1 < input.size()) {
    std::string punctStr = input.substr(start, 2);
    if (punctStr == "==" || punctStr == "!=" || punctStr == "<=" || punctStr == ">=")
      return 2;
  }
  return std::ispunct(input[start]) ? 1 : 0;
}

// EOF parser.
Token *Token::tokenize(std::string &input) {
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
      Cur->setNextToken(createToken(Token::TKind::TK_NUM, It, It));
      Cur = Cur->next();

      auto NumEndPos = getNumberPos(input, It);
      Cur->Val = std::stoi(input.substr(std::distance(input.begin(), It),
                                        std::distance(It, NumEndPos)));
      Cur->Len = std::distance(It, NumEndPos);
      It = NumEndPos;
      continue;
    }

    if (achar >= 'a' && achar <= 'z') {
      Cur->setNextToken(createToken(TKind::TK_IDENT, It, It + 1));
      Cur = Cur->next();
      ++It;
      continue;
    }

    // parse operators.
    int PunctLen = readPunct(input, std::distance(input.begin(), It));
    if (PunctLen) {
      Cur->Next = createToken(Token::TKind::TK_PUNCT, It, It + PunctLen);
      Cur = Cur->Next;
      // move string iterator PunctLen length.
      It += PunctLen;
      continue;
    }

    // unknown character.
    error("invalid token: %c", achar);
  }

  // Add EOF operator.
  Cur->Next = createToken(Token::TKind::TK_EOF, It, It);

  return Head.Next;
}
