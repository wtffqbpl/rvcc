#ifndef TOKENIZE_H
#define TOKENIZE_H

#include <string>

class Token {
public:
  enum class TKind {
    TK_PUNCT, // operator. + -
    TK_NUM,   // number
    TK_EOF,   // end of file
  };

  Token() = default;

  Token(TKind Kind_, Token *Next_, int Val_, std::string::iterator Loc_,
        size_t Len_)
    : Kind(Kind_), Next(Next_), Val(Val_), Loc(Loc_), Len(Len_) {}

  static Token &instance();

  std::string::iterator getNumberPos(std::string &input,
                                            std::string::iterator Start);  
  static Token *tokenize(std::string &input);

private:
  Token *createToken(Token::TKind kind,
                            std::string::iterator Start,
                            std::string::iterator End);


public:
  [[nodiscard]] TKind getKind() const { return Kind; }
  [[nodiscard]] Token *next() const {return Next; }
  [[nodiscard]] int getVal() const { return Val; }
  [[nodiscard]] std::string::iterator getLocation() const { return Loc; }
  [[nodiscard]] std::string::size_type getLength() const { return Len; }

private:
  TKind Kind;                 // kind
  Token *Next;                // next token
  int Val;                    // value
  std::string::iterator Loc;  // location
  std::string::size_type Len; // length
};

#endif