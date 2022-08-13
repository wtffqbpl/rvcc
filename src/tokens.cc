#include "rvcc.h"
#include "tokens.h"
#include <iostream>
#include <string>
#include <unordered_map>

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

TokenContext &TokenContext::instance() {
  static TokenContext TokenCtx;
  return TokenCtx;
}

Token *TokenContext::create(Token::TKind kind, std::string::iterator start,
                            std::string::iterator end) {
  auto *Tok = new Token{kind, nullptr, 0, start,
                        static_cast<size_t>(std::distance(start, end))};
  return Tok;
}

static std::string::iterator getNumber(std::string &input,
                                       std::string::iterator It) {
  for (auto End = input.end(); It != End; ++It) {
    char achar = *It;
    if (!std::isdigit(achar))
      break;
  }
  return It;
}

Token *TokenContext::tokenize(std::string &&input) {
  source_code_ = input;
  Token Head{};
  Token *Cur = &Head;

  auto It = source_code_.begin();
  for (auto End = source_code_.end(); It != End;) {
    auto achar = *It;

    // skip spaces.
    if (std::isspace(achar)) {
      ++It;
      continue;
    }

    // parse number.
    if (std::isdigit(achar)) {
      Cur->setNext(create(Token::TKind::TK_NUM, It, It));
      Cur = Cur->next();

      auto NumEndPos = getNumber(source_code_, It);
      auto Len = std::distance(It, NumEndPos);
      auto Val = std::stoi(
          source_code_.substr(std::distance(source_code_.begin(), It), Len));
      Cur->setVal(Val);
      Cur->setLen(Len);
      It += Len;
      continue;
    }

    if (isValidIndent1(achar)) {
      auto StartPt = It;
      do {
        ++It;
      } while (isValidIndent2(*It));
      Cur->setNext(create(Token::TKind::TK_IDENT, StartPt, It));
      Cur = Cur->next();
      continue;
    }

    // parse operators.
    int PunctLen =
        readPunct(source_code_, std::distance(source_code_.begin(), It));
    if (PunctLen) {
      Cur->setNext(create(Token::TKind::TK_PUNCT, It, It + PunctLen));
      Cur = Cur->next();
      It += PunctLen;
      continue;
    }

    // unknown character.
    logging::error("invalid token: ", achar);
  }

  // Add EOF operator.
  Cur->setNext(create(Token::TKind::TK_EOF, It, It));

  return Head.next();
}