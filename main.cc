#include <iostream>
#include <memory>
#include <stack>
#include <string>
#include <vector>

class Operator {
public:
  Operator();
  enum class Types {
    PLUS,
    MINUS,
    MUL,
    DIV,
    NONE,
  };

  void dump() {
  }

private:
};

template<typename T>
class Value {
  T val;

public:
  void dump() {
#ifndef NDEBUG
    print(std::cout);
#endif
  }

private:
  void print(std::ostream &os) {
    os << "VAL: " << val;
  }
};

template <typename T>
class Instruction {
  std::unique_ptr<Operator> op;
  std::unique_ptr<Value<T>> val1;
  std::unique_ptr<Value<T>> val2;

  void dump() {
#ifndef NDEBUG
    this->print(std::cout);
#endif
  }

private:
  void print(std::ostream &os) {
    val1->dump();
    op->dump();
    val2->dump();
  }
};

class Parser {
  std::vector<std::string> commands;
  std::string raw_input;

public:
  Parser() = default;

  Parser &operator=(const Parser &) = delete;

  static Parser &instance();

  bool parse(std::string &inputs);
};

static Parser *parser = nullptr;

Parser &Parser::instance() {
  if (parser == nullptr) {
    parser = new Parser();
  }

  return *parser;
}

bool Parser::parse(std::string &inputs) {
  if (inputs.size() == 0)
    return false;

#ifndef NDEBUG
  std::cout << inputs << std::endl;
#endif

  std::stack<Operator::Types> opPool;
  std::stack<int> valPool;
  int val = 0;
  Operator::Types op = Operator::Types::NONE;

  auto recordVal = [&](Operator::Types type) {
    opPool.push(type);
    valPool.push(val);
    val = 0;
  };

  for (auto achar : inputs) {
    if (std::isspace(achar))
      continue;

    switch (achar) {
    case '+': {
      recordVal(Operator::Types::PLUS);
      break;
    }
    case '-': {
      recordVal(Operator::Types::MINUS);
      break;
    }
    case '*': {
      recordVal(Operator::Types::MUL);
      break;
    }
    case '/': {
      recordVal(Operator::Types::DIV);
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

  // header
  std::cout << ".globl main\n";
  std::cout << "main:\n";


  std::string delims{" +-"};
  std::string input_val = std::string{argv[1]};
  std::string::size_type begIdx, endIdx;
  std::string::size_type size = input_val.size();

  auto isNumber = [=](std::string const &input,
          const std::string::size_type start,
          const std::string::size_type end)-> bool {
    auto it = std::find_if(input.begin() + start, input.begin() + end,
                           [=](char const &c) -> bool {
      return !std::isdigit(c);
    });
    return !input.empty() && it == (input.begin() + end) && std::isdigit(input[end - 1]);
  };

  // 这里我们将算式分解为 num(op num) (op num) ...  的形式
  endIdx = input_val.find_first_of(delims);
  if (!isNumber(input_val, 0, endIdx == std::string::npos ? size : endIdx)) {
    std::cerr << "This is not a number: " << input_val.substr(0, endIdx) << std::endl;
    std::exit(1);
  }

    std::cout << "  li a0, "
              << input_val.substr(0, endIdx == std::string::npos ? size : endIdx)
              << std::endl;

  begIdx = endIdx;
  while (begIdx != std::string::npos) {
    if (input_val[begIdx] == '+' || input_val[begIdx] == '-') {
      bool negative_flag = input_val[begIdx] == '-';
      ++begIdx;

      bool endflag = false;
      endIdx = input_val.find_first_of(delims, begIdx);
      if (endIdx == std::string::npos) {
        endflag = true;
        endIdx = size;
      }

      if (!isNumber(input_val, begIdx, endIdx)) {
        std::cerr << "This is not a number: " << input_val.substr(begIdx, endIdx) << std::endl;
        std::exit(1);
      }

      std::cout << "  addi a0, a0, "
                << (negative_flag ? "-" : "")
                << input_val.substr(begIdx, endIdx - begIdx) << std::endl;

      begIdx =  endflag ? std::string::npos : endIdx;
      continue;
    }

    std::cerr << "  Unsupported character: " << input_val[begIdx] << std::endl;
    std::exit(1);
  }

  // ret为jalr x0, x1, 0 别名指令，用于返回子程序
  std::cout << "  ret\n";

  return 0;
}
