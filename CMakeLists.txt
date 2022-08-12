cmake_minimum_required(VERSION 3.20)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(rvcc)

enable_testing()

set(CMAKE_CXX_FLAGS "-std=c++2a -g -fno-common -Wno-varargs")

include_directories(src include)
add_executable(rvcc
        src/main.cc
        src/ast_context.cc
        src/codegen.cc
        src/tokens.cc
        )

# testcase directory.
add_subdirectory(tests)