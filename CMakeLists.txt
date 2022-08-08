cmake_minimum_required(VERSION 3.20)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(rvcc)

enable_testing()

set(CMAKE_CXX_FLAGS "-std=c++2a -g -fno-common")

include_directories(src)
add_executable(rvcc
        src/main.cc
        src/astcontext.cc
        src/codegen.cc
        src/tokens.cc
        )

add_test(
  NAME compiler_test
  COMMAND rvcc 1
  )
