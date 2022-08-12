#include "rvcc.h"
#include "tokens.h"
#include <iostream>
#include <string>
#include <unordered_map>

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

/*
 * @brief [a-zA-Z_]
 */
static bool isValidIndent1(char achar) {
  return std::isalpha(achar) || achar == '_';
}

/*
 * @brief [a-zA-Z0-9_]*
 */
static bool isValidIndent2(char achar) {
  return std::isalpha(achar) || std::isdigit(achar) || achar == '_';
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
Token *Token::tokenize(std::string &&input) {
  Token Head{};
  Token *Cur = &Head;

  // SourceCode.swap(input);
  SourceCode = input;
  auto It = SourceCode.begin();

  for (auto End = SourceCode.end(); It != End;) {
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

      auto NumEndPos = getNumberPos(SourceCode, It);
      Cur->Val = std::stoi(SourceCode.substr(std::distance(SourceCode.begin(), It),
                                        std::distance(It, NumEndPos)));
      Cur->Len = std::distance(It, NumEndPos);
      It = NumEndPos;
      continue;
    }

    if (isValidIndent1(achar)) {
      std::string::iterator StartPt = It;
      do {
        ++It;
      } while (isValidIndent2(*It));

      Cur->setNextToken(createToken(TKind::TK_IDENT, StartPt, It));
      Cur = Cur->next();
      continue;
    }

    // parse operators.
    int PunctLen = readPunct(SourceCode, std::distance(SourceCode.begin(), It));
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

static std::string getKindStr(Token::TKind Kind) {
#define TokenTypeName(Kind) {Token::TKind::Kind, #Kind}
  static std::unordered_map<Token::TKind, std::string> KindToStrMap = {
          TokenTypeName(TK_IDENT), TokenTypeName(TK_PUNCT), TokenTypeName(TK_NUM), };

  return KindToStrMap[Kind];
}

void Token::dump(unsigned StatementIndent, unsigned Depth) {
  // terminator.
  if (Kind == Token::TKind::TK_EOF)
    return;

  // Update indent depth.
  ++Depth;

  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";
  std::cout << "{KIND, " << getKindStr(Kind) << "}";
  switch (Kind) {
    case Token::TKind::TK_NUM:
      std::cout << ", {VAL, " << Val << "}";
      break;
    case Token::TKind::TK_PUNCT: {
      std::string_view TokName = getTokenName();
      std::cout << ", {SIGN, " << TokName << "}";
      if (TokName == ";")
        Depth = StatementIndent;
      break;
    }
    case Token::TKind::TK_IDENT:
      std::cout << ", {NAME, " << getTokenName() << "}";
      break;
    default:
      assert("No this type of token.");
      break;
  }
  std::cout << std::endl;

  Next->dump(StatementIndent, Depth);
}
