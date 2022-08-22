#ifndef TOKENIZE_H
#define TOKENIZE_H

#include <iostream>
#include <string>
#include <unordered_map>

class Token {
public:
  enum class TKind {
    TK_IDENT,   // ident
    TK_PUNCT,   // operator. + -
    TK_KEYWORD, // KeyWord.
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

  void dump(unsigned StatementIndent = 0, unsigned Depth = 0);

protected:
  virtual void print(std::ostream &os) {}

protected:
  TKind Kind_;            // kind
  Token *Next_ = nullptr; // next token
  size_t Len_;            // length
};

// KeyWord token class.
class KeyWordToken : public Token {
public:
  enum class KeyWordT : uint8_t {
#define C_KEYWORD_INFO(KeyWord, Expr, Desc) KT_##KeyWord,
#include "c_syntax_info.def"
  };

public:
  explicit KeyWordToken(std::string_view KeyWord)
      : Token(Token::TKind::TK_KEYWORD, nullptr, KeyWord.size()),
        KeyWord_(KeyWord) {}
  
  [[nodiscard]] std::string_view getKeyWordName() const {return KeyWord_;}

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_KEYWORD;
  }

  static bool isKeyWord(std::string &InKeyWord) {
    return StrKeyWordMap_.find(InKeyWord) != StrKeyWordMap_.end();
  }

private:
  // @brief for token debug : keyword type  -> string
  static std::unordered_map<KeyWordToken::KeyWordT, std::string> KeyWordStrMap_;
  
  // @brief for token debug : string -> keyword type.
  static std::unordered_map<std::string, KeyWordToken::KeyWordT> StrKeyWordMap_;

  std::string_view KeyWord_;
};

// 数字Token
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

// 运算符Token
class PunctToken : public Token {
public:
  explicit PunctToken(std::string_view Name)
      : Token(Token::TKind::TK_PUNCT, nullptr, Name.size()), Name_(Name) {}

  [[nodiscard]] std::string_view getName() { return Name_; }

  void print(std::ostream &os) override { os << ", {SIGN, " << Name_ << "}"; }

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_PUNCT;
  }

private:
  std::string_view Name_;
};

// 字符Token
class IndentToken : public Token {
public:
  explicit IndentToken(std::string_view Name)
      : Token(Token::TKind::TK_IDENT, nullptr, Name.size()), Name_(Name) {}

  [[nodiscard]] std::string_view getName() const { return Name_; }

  void print(std::ostream &os) override { os << ", {SIGN, " << Name_ << "}"; }

public:
  static bool isa(const Token *V) {
    return V->getKind() == Token::TKind::TK_IDENT;
  }

private:
  std::string_view Name_;
};

// 结束符Token
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
template <typename ET> bool isa(Token *T) { return ET::isa(T); }

#endif