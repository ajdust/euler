#include "revc.hpp"
#include <algorithm>
#include <iostream>
#include <sstream>

void REVC_unreversed(std::istream& in_dna, std::ostream& out_to)
{
    std::string line;
    while (std::getline(in_dna, line)) {
        for (char& c : line) {
            if (c == 'A')
                out_to << 'T';
            else if (c == 'C')
                out_to << 'G';
            else if (c == 'G')
                out_to << 'C';
            else if (c == 'T')
                out_to << 'A';
        }
    }
}

void REVC(std::istream& in_dna, std::ostream& out_to)
{
    std::stringstream ss{};
    REVC_unreversed(in_dna, ss);
    std::string reversed{ ss.str() };
    std::reverse(reversed.begin(), reversed.end());
    out_to << reversed;
}
