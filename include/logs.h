//
// Created by CL on 2022/8/15.
//

#ifndef RVCC_LOGS_H
#define RVCC_LOGS_H


#include <iostream>

namespace logging {

#define LOG_LEVEL 1

template <typename T> void print(T Val) { std::cout << Val << " "; }

template <typename T, typename... Types>
void print(T FirstArg, Types... Lefts) {
	print(FirstArg);
	print(Lefts...);
}

template <typename T, typename... Types>
void error(T FirstArg, Types... Lefts) {
	std::cout << "[ERROR]: ";
	print(FirstArg, Lefts...);
	std::cout << std::endl;

	// terminate program.
	std::exit(1);
}

template <typename T, typename... Types> void info(T FirstArg, Types... Lefts) {
	std::cout << "[INFO]: ";
	print(FirstArg, Lefts...);
	std::cout << std::endl;
}

} // end of namespace logging.


#endif //RVCC_LOGS_H
