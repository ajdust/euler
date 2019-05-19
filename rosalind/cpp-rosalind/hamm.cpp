#include "hamm.hpp"
#include <algorithm>
#include <sstream>
#include <string>

int hamming_distance(std::string a, std::string b)
{
    int distance{ 0 };
    auto it_a{ a.begin() };
    auto it_b{ b.begin() };

    while (it_a != a.end() || it_b != b.end()) {
        if (*it_a != *it_b)
            distance += 1;
        it_a++;
        it_b++;
    }

    return distance;
}

void HAMM(std::istream& input, std::ostream& output)
{
    std::string a, b;
    std::getline(input, a);
    std::getline(input, b);
    output << hamming_distance(a, b);
}
