//
// Created by CL on 2022/8/15.
//

#ifndef RVCC_LOGS_H
#define RVCC_LOGS_H

#include <iostream>

namespace logging {

#define LOG_LEVEL 1

template <typename T> void print(T Val) { std::cout << Val << " "; }

template <typename T, typename... Types>
void print(T FirstArg, Types... Lefts) {
  print(FirstArg);
  print(Lefts...);
}

template <typename T, typename... Types>
static void error_internal(T FirstArg, Types... Lefts) {
  std::cout << "[ERROR] ";
  print(FirstArg, Lefts...);
  std::cout << std::endl;

  // terminate program.
  std::exit(1);
}

#define error(...)                                                             \
  error_internal(__FILE__, ", line=", static_cast<size_t>(__LINE__), ": ",     \
                 __VA_ARGS__)

template <typename T, typename... Types>
static void unreachable_internal(T Msg, Types... Lefts) {
  std::cout << "[UNREACHABLE]: ";
  print(Msg, Lefts...);
  std::cout << std::endl;

  // terminate program.
  std::exit(2);
}

#define unreachable(...)                                                       \
  unreachable_internal(__FILE__, "line=", static_cast<size_t>(__LINE__), ": ", \
                       __VA_ARGS__)

template <typename T, typename... Types>
static void info_internal(T FirstArg, Types... Lefts) {
  std::cout << "[INFO]: ";
  print(FirstArg, Lefts...);
  std::cout << std::endl;
}

#define info(...)                                                              \
  info_internal(__FILE__, ", line = ", static_cast<size_t>(__LINE__), ": ",    \
                __VA_ARGS)

} // end of namespace logging.

#endif //RVCC_LOGS_H
