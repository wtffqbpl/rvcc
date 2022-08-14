cmake_minimum_required(VERSION 3.16)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(rvcc)

enable_testing()

set(CMAKE_CXX_FLAGS "-std=c++2a -g -fno-common")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/build")

add_executable(rvcc 
  src/main.cpp
  src/codegen.cpp
  src/parse.cpp
  src/tokenize.cpp
)


add_test(
  NAME compiler_test
  COMMAND rvcc 1
  )
