cmake_minimum_required(VERSION 3.20)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(rvcc)

enable_testing()

add_executable(rvcc main.cc)

add_test(
  NAME compiler_test
  COMMAND rvcc 1
  )
