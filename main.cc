#include <iostream>
#include <string>

int main(int argc, char **argv) {
  if (argc != 2) {
    std::cerr << "Invalid arguments.\n";
    std::cerr << "Usage:\n";
    std::cerr << "\t ./exec val\n";
    return 1;
  }

  // 声明一个全局main段，同时也是程序入口段
  std::cout << ".globl main\n";

  // main段标签
  std::cout << "main:\n";
  // li 为addi指令的别名，加载一个立即数到寄存器中
  // 传入程序的参数为str类型，因为需要转换为int类型
  // atoi 为 ascii to integer
  std::cout << "  li a0, " << atoi(argv[1]) << "\n";

  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  ret\n";

  return 0;
}
