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

  Token(TKind Kind_, Token *Next_, int Val_, std::string::iterator Loc_,
        size_t Len_)
          : Kind(Kind_), Next(Next_), Val(Val_), Loc(Loc_), Len(Len_) {}

  static Token &instance();
  Token *tokenize(std::string &&input);

private:
  Token *createToken(Token::TKind kind,
                     std::string::iterator Start,
                     std::string::iterator End);
  std::string::iterator getNumberPos(std::string &input,
                                     std::string::iterator Start);

public:
  [[nodiscard]] TKind getKind() const { return Kind; }
  [[nodiscard]] Token *next() const {return Next; }
  void setNextToken(Token *Tok_) { Next = Tok_; }
  [[nodiscard]] int getVal() const { return Val; }
  [[nodiscard]] std::string::iterator getLocation() const { return Loc; }
  [[nodiscard]] std::string::size_type getLength() const { return Len; }
  [[nodiscard]] std::string_view getTokenName() {
    std::string::size_type Start = std::distance(SourceCode.begin(), Loc);
    return std::string_view{SourceCode.data() + Start, Len};
  }

  void dump(unsigned StatementIndent=0, unsigned Depth=0);

private:
  std::string SourceCode;                         // source code
  TKind Kind = Token::TKind::TK_EOF;              // kind
  Token *Next = nullptr;                          // next token
  int Val = 0;                                    // value
  std::string::iterator Loc;                      // location
  std::string::size_type Len = std::string::npos; // length
};

#endif  // SRC_TOKENS_H