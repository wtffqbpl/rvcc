cmake_minimum_required(VERSION 3.16)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

project(rvcc)

enable_testing()

set(CMAKE_CXX_FLAGS "-std=c++2a -g -fno-common")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${PROJECT_SOURCE_DIR}/build")

include_directories(src, include)
add_executable(rvcc 
  src/main.cpp
  src/CGAsm.cpp
  src/ASTBaseNode.cpp
  src/tokenize.cpp
  src/ASTContext.cpp
  src/BasicObjects.cpp
  include/my_timer.h
  include/logs.h
  include/rvcc.h
  )


# testcase directory.
add_subdirectory(tests)