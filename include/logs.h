#ifndef SRC_LOGS_H
#define SRC_LOGS_H

#include <iostream>

namespace logging {

template <typename T> void print(T Val) { std::cout << Val << " "; }

template <typename T, typename... Types>
void print(T FirstArg, Types... Lefts) {
  print(FirstArg);
  print(Lefts...);
}

template <typename T, typename... Types>
void error_internal(T FirstArg, Types... Lefts) {
  std::cout << "[ERROR] ";
  print(FirstArg, Lefts...);
  std::cout << std::endl;

  // terminate program.
  std::exit(1);
}

template <typename T, typename... Types>
void unreachable_internal(T Msg, Types... Lefts) {
  std::cout << "[UNREACHABLE]: ";
  print(Msg, Lefts...);
  std::cout << std::endl;

  // terminate program.
  std::exit(2);
}

template <typename T, typename... Types>
void info_internal(T FirstArg, Types... Lefts) {
  std::cout << "[INFO]: ";
  print(FirstArg, Lefts...);
  std::cout << std::endl;
}

//########################### External APIs ###############################

#define error(...)                                                             \
  error_internal(__FILE__, ", line=", static_cast<size_t>(__LINE__), ": ",     \
                 __VA_ARGS__)

#define unreachable(...)                                                       \
  unreachable_internal(__FILE__, "line=", static_cast<size_t>(__LINE__), ": ", \
                       __VA_ARGS__)

#define info(...)                                                              \
  info_internal(__FILE__, ", line=", static_cast<size_t>(__LINE__), ": ",      \
                __VA_ARGS__)

} // end of namespace logging.

#endif // SRC_LOGS_H
