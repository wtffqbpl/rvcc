//
// Created by CL on 2022/8/29.
//

#ifndef RVCC_INCLUDE_C_SYNTAX_H
#define RVCC_INCLUDE_C_SYNTAX_H
namespace c_syntax {
// c syntax keywords enum definition
enum class CKType : unsigned {
#define C_KEYWORD_INFO(Keyword, Expr, Desc) CK_##Keyword,
#include "c_syntax_info.def"
};
} // namespace c_syntax

#endif // RVCC_INCLUDE_C_SYNTAX_H
