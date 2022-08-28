#include "tokenize.h"
#include "c_syntax.h"
#include "rvcc.h"
#include <cassert>
#include <cstdarg>
#include <iostream>
#include <map>
#include <memory>
#include <stack>
#include <string>

#define DEBUG_TYPE "tokens"

std::map<const std::string_view, c_cyntax::CKType>
    KeywordToken::StrKeywordMap_ = {
#define C_KEYWORD_INFO(Keyword, Expr, Desc)                                    \
  {Expr, c_cyntax::CKType::CK_##Keyword},
#include "c_syntax_info.def"
};

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
  static std::map<Token::TKind, std::string> KindToStrMap = {
      TokenTypeName(TK_IDENT),
      TokenTypeName(TK_PUNCT),
      TokenTypeName(TK_NUM),
      TokenTypeName(TK_KEYWORD),
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
  std::cout << "{" << getKindStr(Kind_);
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
  case Token::TKind::TK_KEYWORD:
    dynamic_cast<KeywordToken *>(this)->print(std::cout);
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
  std::string_view::size_type Len = std::distance(Start, End);
  auto Offset = std::distance(SourceCode_.begin(), Start);
  Token *Tok = nullptr;
  switch (Kind) {
  case Token::TKind::TK_NUM: {
    auto Val = std::stoi(SourceCode_.substr(Offset, Len));
    Tok = new NumToken{Val, static_cast<size_t>(Len)};
    break;
  }
  case Token::TKind::TK_PUNCT: {
    std::string_view Name = std::string_view(SourceCode_.data() + Offset, Len);
    Tok = new PunctToken{std::move(Name)};
    break;
  }
  case Token::TKind::TK_IDENT: {
    std::string_view Name = std::string_view(SourceCode_.data() + Offset, Len);
    Tok = new IndentToken{std::move(Name)};
    break;
  }
  case Token::TKind::TK_KEYWORD: {
    std::string_view Name = std::string_view(SourceCode_.data() + Offset, Len);
    Tok = new KeywordToken{std::move(Name)};
    break;
  }
  case Token::TKind::TK_EOF:
    Tok = new EOFToken{};
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
  SourceCode_ = input;
  Token Head{};
  Token *Cur = &Head;

  auto It = SourceCode_.begin();
  for (auto End = SourceCode_.end(); It != End;) {
    char achar = *It;

    // skip spaces.
    if (std::isspace(achar)) {
      ++It;
      continue;
    }

    // parse number.
    if (std::isdigit(achar)) {
      auto NumEndPos = getNumber(SourceCode_, It);
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
      // we need check the Indent is variable or keyword there.
      std::string VarName =
          SourceCode_.substr(std::distance(SourceCode_.begin(), StartPt),
                             std::distance(StartPt, It));
      if (KeywordToken::isKeyWord(VarName)) {
        Cur->setNext(create(Token::TKind::TK_KEYWORD, StartPt, It));
      } else {
        Cur->setNext(create(Token::TKind::TK_IDENT, StartPt, It));
      }
      Cur = Cur->next();
      continue;
    }

    // parse operators.
    int PunctLen =
        readPunct(SourceCode_, std::distance(SourceCode_.begin(), It));
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
