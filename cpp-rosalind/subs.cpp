#include "subs.hpp"
#include <iostream>
#include <string>
#include <vector>

std::vector<unsigned long> find_substrs(std::string str, std::string substr)
{
    unsigned long pos = 0;
    auto i = str.find(substr, pos);
    std::vector<unsigned long> indexes{};
    while (i != std::string::npos) {
        indexes.push_back(i);
        pos = i + 1;
        i = str.find(substr, pos);
    }

    return indexes;
}

void SUBS(std::istream& input, std::ostream& output)
{
    std::string dna{};
    std::string motif{};
    std::getline(input, dna);
    std::getline(input, motif);
    auto indexes = find_substrs(dna, motif);
    for (auto i : indexes)
        output << i + 1 << " ";
}
