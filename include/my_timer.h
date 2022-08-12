#ifndef INCLUDE_TIMER_H
#define INCLUDE_TIMER_H

#include <chrono>
#include <iostream>
#include <string_view>

class Timer {
public:
  Timer(const std::string_view Title_) : Title(Title_) {
    Start = std::chrono::high_resolution_clock::now();
  }

  ~Timer() { stop(); }

private:
  void stop() {
    auto Stop = std::chrono::high_resolution_clock::now();
    std::chrono::milliseconds ms =
            std::chrono::duration_cast<std::chrono::milliseconds>(Stop - Start);
    std::cout << Title << " " << ms.count() * 0.001 << "s\n";
  }

private:
  std::string_view Title;
  std::chrono::high_resolution_clock::time_point Start;
};

#endif // INCLUDE_TIMER_H
