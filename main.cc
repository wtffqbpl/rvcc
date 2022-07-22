#include <iostream>
#include <stack>
#include <string>
#include <vector>

class Parser {
  std::vector<std::string> commands;
  std::string raw_input;

  enum class Operator {
    PLUS,
    MINUS,
    MUL,
    DIV,
    NONE,
  };

public:
  Parser() = default;

  Parser &operator=(const Parser &) = delete;

  Parser &instance();

  bool parse();
};

static Parser *parser = nullptr;

Parser &Parser::instance() {
  if (parser == nullptr) {
    parser = new Parser();
  }

  return *parser;
}

bool Parser::parse() {
  if (raw_input.size() == 0)
    return false;

  std::stack<char> pool;
  int val = 0;
  Operator op = Operator::NONE;

  for (auto achar : raw_input) {
    switch (achar) {
    case '+': {
      op = Operator::PLUS;
      pool.push(val);
      val = 0;
      break;
    }
    case '-': {
      op = Operator::MINUS;
      pool.push(val);
      val = 0;
      break;
    }
    default: {
      if (std::isdigit(achar))
        val = val * 10 + achar - '0';
      else if (std::isalpha(achar)) {
        std::cout << "Unsupported yet." << std::endl;
        ::abort();
      }
      break;
    }
    }
  }
  return true;
}

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
