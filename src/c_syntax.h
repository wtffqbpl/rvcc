#ifndef SRC_C_SYNTAX_H
#define SRC_C_SYNTAX_H

namespace c_syntax {
// c syntax keywords enum definition.
enum class CKType : unsigned {
#define C_KEYWORD_INFO(Keyword, Expr, Desc) CK_##Keyword,
#include "c_syntax_info.def"
};

enum class PTType : unsigned {
#define C_PUNCTUATION_INFO(Keyword, Expr, Desc) PT_##Keyword,
#include "c_syntax_info.def"
};

} // end of namespace c_syntax

#endif // SRC_C_SYNTAX_H
