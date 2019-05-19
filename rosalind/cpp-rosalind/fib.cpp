#include "fib.hpp"
#include <iostream>

// With n generations, each generation producing k
long FIB(int n, int k)
{
    long previous{ 0 }, current{ 1 };

    for (int i{ 0 }; i < n - 1; ++i) {
        // typical fibonacci:
        // (a, b) = (b, a + b)
        // 1 pair makes k pairs:
        // (prev_gen, current_gen) = (current_gen, prev_gen * k + current_gen)
        long temp{ current };
        current += previous * k;
        previous = temp;
    }

    return current;
}

void FIB(std::istream& input, std::ostream& output)
{
    std::string line;
    std::getline(input, line);
    auto i{ line.find(" ") };
    int n{ std::stoi(line.substr(0, i)) };
    int k{ std::stoi(line.substr(i + 1, line.length())) };
    output << FIB(n, k);
}
