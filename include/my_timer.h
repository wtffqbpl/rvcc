//
// Created by CL on 2022/8/15.
//

#ifndef RVCC_MY_TIMER_H
#define RVCC_MY_TIMER_H


#include <chrono>
#include <iostream>
#include <string_view>

class Timer {
public:
  Timer(const std::string_view &Title) : Title_(Title) {
    Start_ = std::chrono::high_resolution_clock::now();
  }

  ~Timer() { stop(); }

private:
  void stop() {
    auto Stop = std::chrono::high_resolution_clock::now();
    std::chrono::nanoseconds Ms =
        std::chrono::duration_cast<std::chrono::nanoseconds>(Stop - Start_);
    std::cout << Title_ << " " << Ms.count() << "ns\n";
  }

private:
  const std::string_view &Title_;
  std::chrono::high_resolution_clock::time_point Start_;
};


#endif //RVCC_MY_TIMER_H
