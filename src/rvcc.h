#ifndef SRC_RVCC_H
#define SRC_RVCC_H

#include "../include/logs.h"
#include <cassert>

/*
 * @brief Align N to Align.
 */
static inline int alignTo(int N, int Align) {
  // align to Align. (0, Align] ---> return Align
  return (N + Align - 1) / Align * Align;
}

#endif // SRC_RVCC_H
