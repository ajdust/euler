#include "rna.hpp"
#include <iostream>

void RNA(std::istream& in_dna, std::ostream& out_rna)
{
    std::string line;
    while (std::getline(in_dna, line)) {
        for (char& c : line) {
            if (c == 'A')
                out_rna << 'A';
            else if (c == 'C')
                out_rna << 'C';
            else if (c == 'G')
                out_rna << 'G';
            else if (c == 'T')
                out_rna << 'U';
        }
    }
}
