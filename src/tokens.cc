#include "tokens.h"
#include "c_syntax.h"
#include "rvcc.h"
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>

#define DEBUG_TYPE "tokens"

std::map<const std::string_view, c_syntax::CKType>
    KeywordToken::StrKeywordMap_ = {
#define C_KEYWORD_INFO(Keyword, Expr, Desc)                                    \
  {Expr, c_syntax::CKType::CK_##Keyword},
#include "c_syntax_info.def"
};

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

Token *TokenContext::create(Token::TKind Kind, std::string::iterator Start,
                            std::string::iterator End) {
  std::string_view::size_type Len = std::distance(Start, End);
  auto Offset = std::distance(source_code_.begin(), Start);

  Token *Tok = nullptr;
  switch (Kind) {
  case Token::TKind::TK_NUM: {
      auto Val = std::stoi(source_code_.substr(Offset, Len));
      Tok = new NumToken{Val, static_cast<size_t>(Len)};
      break;
  }
  case Token::TKind::TK_PUNCT: {
      std::string Name{source_code_.data() + Offset, Len};
      Tok = new PunctToken{Name};
      break;
  }
  case Token::TKind::TK_IDENT: {
      std::string Name{source_code_.data() + Offset, Len};
      Tok = new IndentToken{Name};
      break;
  }
  case Token::TKind::TK_KEYWORD: {
      std::string Name{source_code_.data() + Offset, Len};
      Tok = new KeywordToken{Name};
      break;
  }
  case Token::TKind::TK_EOF:
    Tok = new EOFToken{};
    break;
  default:
    // logging::error("Unknown token type: ",
    // std::underlying_type<unsigned>::Kind));
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

Token *TokenContext::tokenize(std::string &&input) {
  // record source code in static TokenContext class.
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
      auto NumEndPos = getNumber(source_code_, It);
      auto *Tok = create(Token::TKind::TK_NUM, It, NumEndPos);
      Cur->setNext(Tok);
      tokens_.emplace_back(Tok);

      Cur = Cur->next();
      It += Cur->getLength();
      continue;
    }

    if (isValidIndent1(achar)) {
      auto StartPt = It;
      do {
        ++It;
      } while (isValidIndent2(*It));

      std::string VarName =
          source_code_.substr(std::distance(source_code_.begin(), StartPt),
                              std::distance(StartPt, It));
      if (KeywordToken::isKeyword(VarName)) {
        auto *Tok = create(Token::TKind::TK_KEYWORD, StartPt, It);
        Cur->setNext(Tok);
        tokens_.emplace_back(Tok);
      } else {
        auto *Tok = create(Token::TKind::TK_IDENT, StartPt, It);
        Cur->setNext(Tok);
        tokens_.emplace_back(Tok);
      }
      Cur = Cur->next();
      continue;
    }

    // parse operators.
    int PunctLen =
        readPunct(source_code_, std::distance(source_code_.begin(), It));
    if (PunctLen) {
      auto *Tok = create(Token::TKind::TK_PUNCT, It, It + PunctLen);
      Cur->setNext(Tok);
      Cur = Cur->next();
      It += PunctLen;

      tokens_.emplace_back(Tok);
      continue;
    }

    // unknown character.
    logging::error("invalid token: ", achar);
  }

  // Add EOF operator.
  {
    auto *Tok = create(Token::TKind::TK_EOF, It, It);
    Cur->setNext(Tok);
    tokens_.emplace_back(Tok);
  }

  return Head.next();
}
