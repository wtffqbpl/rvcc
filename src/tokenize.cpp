#include "tokenize.h"
#include "rvcc.h"
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>

// @brief [a-z,A-Z]
static bool isValidIndent_1(char achar) {
  return std::isalpha(achar) || achar == '_';
}

// @brief [a-z,A-Z]
static bool isValidIndent_2(char achar) {
  return std::isalpha(achar) || std::isdigit(achar) || achar == '_';
}

static std::string getKindStr(Token::TKind Kind) {
#define TokenTypeName(Kind)                                                    \
  { Token::TKind::Kind, #Kind }
  static std::unordered_map<Token::TKind, std::string> KindToStrMap = {
      TokenTypeName(TK_IDENT),
      TokenTypeName(TK_PUNCT),
      TokenTypeName(TK_NUM),
  };
  return KindToStrMap[Kind];
}

void Token::dump(unsigned StatementIndent, unsigned Depth) {
  // terminator.
  if (Kind_ == Token::TKind::TK_EOF)
    return;

  // Update indent depth.
  ++Depth;

  for (unsigned i = 0; i < Depth; ++i)
    std::cout << "  ";
  std::cout << "{KIND, " << getKindStr(Kind_) << "}";
  switch (Kind_) {
  case Token::TKind::TK_NUM:
    dynamic_cast<NumToken *>(this)->print(std::cout);
    break;
  case Token::TKind::TK_PUNCT: {
    auto Tok = dynamic_cast<PunctToken *>(this);
    Tok->print(std::cout);
    if (Tok->getName() == ";")
      Depth = StatementIndent;
    break;
  }
  case Token::TKind::TK_IDENT:
    dynamic_cast<IndentToken *>(this)->print(std::cout);
    break;
  default:
    assert("No this type of token.");
    break;
  }
  std::cout << std::endl;

  Next_->dump(StatementIndent, Depth);
}

TokenContext &TokenContext::instance() {
  static TokenContext TokenCtx;
  return TokenCtx;
}

// Parse string with position, and generate token
Token *TokenContext::create(Token::TKind Kind, std::string::iterator Start,
                            std::string::iterator End) {
  auto Len = std::distance(Start, End);
  auto Offset = std::distance(source_code_.begin(), Start);
  Token *Tok = nullptr;
  switch (Kind) {
  case Token::TKind::TK_NUM: {
    auto Val = std::stoi(source_code_.substr(Offset, Len));
    Tok = dynamic_cast<Token *>(new NumToken{Val, static_cast<size_t>(Len)});
    break;
  }
  case Token::TKind::TK_PUNCT: {
    std::string_view Name = std::string_view(source_code_.data() + Offset, Len);
    Tok = dynamic_cast<Token *>(new PunctToken{Name});
    break;
  }
  case Token::TKind::TK_IDENT: {
    std::string_view Name = std::string_view(source_code_.data() + Offset, Len);
    Tok = dynamic_cast<Token *>(new IndentToken{Name});
    break;
  }
  case Token::TKind::TK_EOF:
    Tok = dynamic_cast<Token *>(new EOFToken{});
    break;
  default:
    logging::error("Unknown token type: ", static_cast<unsigned>(Kind));
    break;
  }

  return Tok;
}

static std::string::iterator getNumber(std::string &input,
                                       std::string::iterator It) {
  for (auto End = input.end(); It != End; ++It) {
    if (!std::isdigit(*It))
      break;
  }
  return It;
}

// EOF parser.
Token *TokenContext::tokenize(std::string &&input) {
  // The feature of std::move was used for parameter "&&input".
  source_code_ = input;
  Token Head{};
  Token *Cur = &Head;

  auto It = source_code_.begin();
  for (auto End = source_code_.end(); It != End;) {
    char achar = *It;

    // skip spaces.
    if (std::isspace(achar)) {
      ++It;
      continue;
    }

    // parse number.
    if (std::isdigit(achar)) {
      auto NumEndPos = getNumber(source_code_, It);
      Cur->setNext(create(Token::TKind::TK_NUM, It, NumEndPos));
      Cur = Cur->next();
      It += Cur->getLength();
      continue;
    }

    // parse single char variable
    if (isValidIndent_1(achar)) {
      auto StartPt = It;
      do {
        ++It;
      } while (isValidIndent_2(*It));
      Cur->setNext(create(Token::TKind::TK_IDENT, StartPt, It));
      Cur = Cur->next();
      continue;
    }

    // parse operators.
    int PunctLen =
        readPunct(source_code_, std::distance(source_code_.begin(), It));
    if (PunctLen) {
      // move string iterator PunctLen length.
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
