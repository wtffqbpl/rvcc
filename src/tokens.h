#ifndef SRC_TOKENS_H
#define SRC_TOKENS_H

#include "c_syntax.h"
#include <iostream>
#include <map>
#include <string_view>

class Token {
public:
  enum class TKind {
    TK_IDENT,   // identifier, 可以是变量名，函数名等
    TK_PUNCT,   // operator. + -
    TK_KEYWORD, // keyword. Such as int, return etc.
    TK_NUM,     // number
    TK_EOF,     // end of file
  };

  Token() = default;
  Token(const Token &) = delete;
  Token(Token &&) = delete;
  Token &operator=(const Token &) = delete;
  Token &operator=(Token &&) = delete;

  Token(TKind Kind, Token *Next, size_t Len)
      : Kind_(Kind), Next_(Next), Len_(Len) {}

public:
  [[nodiscard]] TKind getKind() const { return Kind_; }
  [[nodiscard]] Token *next() const { return Next_; }
  void setNext(Token *Tok_) { Next_ = Tok_; }
  [[nodiscard]] size_t getLength() const { return Len_; }

  void dump(unsigned StatementIndent=0, unsigned Depth=0);

protected:
  virtual void print(std::ostream &os) {}

protected:
  TKind Kind_;            // kind
  Token *Next_ = nullptr; // next token
  size_t Len_;            // length
};

class KeywordToken : public Token {
public:
  explicit KeywordToken(const std::string_view &&Keyword)
      : Token(TKind::TK_KEYWORD, nullptr, Keyword.size()), Keyword_(Keyword),
        KeywordType_(getKeywordTypeByName(Keyword_)) {}
  [[nodiscard]] const std::string_view &getKeywordName() const {
    return Keyword_;
  }

public:
  static bool isa(const Token *V) { return V->getKind() == TKind::TK_KEYWORD; }

  static bool isKeyword(const std::string_view &InKeyword) {
    return StrKeywordMap_.contains(InKeyword);
  }

  [[nodiscard]] c_syntax::CKType getKeywordType() const { return KeywordType_; }

private:
  static c_syntax::CKType
  getKeywordTypeByName(const std::string_view &Keyword) {
    return StrKeywordMap_[Keyword];
  }

private:
  // @brief for token parse: string -> keyword type
  static std::map<const std::string_view, c_syntax::CKType> StrKeywordMap_;
  const std::string_view Keyword_;
  c_syntax::CKType KeywordType_;
};

class NumToken : public Token {
public:
  explicit NumToken(int Val, size_t Len)
      : Token(Token::TKind::TK_NUM, nullptr, Len), Val_(Val) {}

  [[nodiscard]] int getVal() const { return Val_; }

  void print(std::ostream &os) override { os << ", {VAL, " << Val_ << "}"; }

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_NUM;
  }

private:
  int Val_;
};

class PunctToken : public Token {
public:
  enum class PunctTy : unsigned {
    PT_L_BRACE,
    PT_R_BRACE,
    PT_L_PAREN,
    PT_R_PAREN,
    PT_L_MID_PAREN,
    PT_R_MID_PAREN,
    PT_EQ,
    PT_LT,
    PT_LE,
    PT_GT,
  };

public:
  explicit PunctToken(const std::string_view &&Name)
      : Token(Token::TKind::TK_PUNCT, nullptr, Name.size()), Name_(Name) {}

  [[nodiscard]] const std::string_view &getName() { return Name_; }

  void print(std::ostream &os) override { os << ", {SIGN, " << Name_ << "}"; }

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_PUNCT;
  }

private:
  const std::string_view Name_;
};

class IndentToken : public Token {
public:
  explicit IndentToken(const std::string_view &&Name)
      : Token(Token::TKind::TK_IDENT, nullptr, Name.size()), Name_(Name) {}

  [[nodiscard]] std::string_view getName() const { return Name_; }

  void print(std::ostream &os) override { os << ", {SIGN, " << Name_ << "}"; }

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_IDENT;
  }

private:
  const std::string_view Name_;
};

class EOFToken : public Token {
public:
  explicit EOFToken() : Token(Token::TKind::TK_EOF, nullptr, 0) {}

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_EOF;
  }
};

class TokenContext {
public:
  TokenContext() = default;
  static TokenContext &instance();
  Token *tokenize(std::string &&input);

private:
  Token *create(Token::TKind kind, std::string::iterator start,
                std::string::iterator end);

private:
  std::string SourceCode_;
};

//############################### Some Utils. #################################
/*
 * @brief 调用每个类型中的isa 静态函数，来判断传入的实例是否是期待的类型.
 */
template <typename ET> bool isa(const Token *T) { return ET::isa(T); }

#endif  // SRC_TOKENS_H
