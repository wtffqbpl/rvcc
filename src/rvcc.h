#ifndef SRC_RVCC_H
#define SRC_RVCC_H

#include <cassert>
#include <cstdarg>
#include <string>

static void error(std::string StrFmt, ...) {
  // define a va_list variable.
  va_list VA;
  const char *Fmt = StrFmt.data();
  // get all arguments after Fmt.
  va_start(VA, Fmt);
  // output va_list variable using vfprintf.
  vfprintf(stderr, Fmt, VA);
  // add line separator.
  fprintf(stderr, "\n");
  // clear VA
  va_end(VA);
  // terminate program.
  std::exit(1);
}

/*
 * @brief Align N to Align.
 */
static inline int alignTo(int N, int Align) {
  // align to Align. (0, Align] ---> return Align
  return (N + Align - 1) / Align * Align;
}

#endif // SRC_RVCC_H
