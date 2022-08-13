#ifndef SRC_TOKENS_H
#define SRC_TOKENS_H

class Token {
public:
  enum class TKind {
    TK_IDENT, // identifier, 可以是变量名，函数名等
    TK_PUNCT, // operator. + -
    TK_NUM,   // number
    TK_EOF,   // end of file
  };

  Token() = default;
  Token(const Token &) = delete;

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

template <typename ET> bool isa(Token *V) {
  try {
    auto *Tok = dynamic_cast<ET *>(V);
    if (Tok && ET::isa(Tok))
      return true;
  } catch (...) {
    // No need to do anything.
  }
  return false;
}

class NumToken : public Token {
public:
  explicit NumToken(int Val, size_t Len)
      : Token(Token::TKind::TK_NUM, nullptr, Len), Val_(Val) {}

  [[nodiscard]] int getVal() const { return Val_; }

  void print(std::ostream &os) override { os << ", {VAL, " << Val_ << "}"; }

public:
  static bool isa(const NumToken *V) {
    return V->getKind() == Token::TKind::TK_NUM;
  }

private:
  int Val_ = 0;
};

class PunctToken : public Token {
public:
  explicit PunctToken(std::string_view Name)
      : Token(Token::TKind::TK_PUNCT, nullptr, Name.size()), Name_(Name) {}

  [[nodiscard]] std::string_view getName() { return Name_; }

  void print(std::ostream &os) override { os << ", {SIGN, " << Name_ << "}"; }

public:
  static bool isa(const PunctToken *V) {
    return V->getKind() == Token::TKind::TK_PUNCT;
  }

private:
  std::string_view Name_;
};

class IndentToken : public Token {
public:
  explicit IndentToken(std::string_view Name)
      : Token(Token::TKind::TK_IDENT, nullptr, Name.size()), Name_(Name) {}

  [[nodiscard]] std::string_view getName() const { return Name_; }

  void print(std::ostream &os) override { os << ", {SIGN, " << Name_ << "}"; }

public:
  static bool isa(const IndentToken *V) {
    return V->getKind() == Token::TKind::TK_IDENT;
  }

private:
  std::string_view Name_;
};

class EOFToken : public Token {
public:
  explicit EOFToken() : Token(Token::TKind::TK_EOF, nullptr, 0) {}

public:
  static bool isa(const EOFToken *V) {
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
  std::string source_code_;
};

#endif  // SRC_TOKENS_H
