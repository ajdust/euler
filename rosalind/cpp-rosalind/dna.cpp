#include "dna.hpp"
#include <iostream>

DnaBaseCount DNA_count(std::istream& in_dna)
{
    int A{ 0 }, C{ 0 }, G{ 0 }, T{ 0 };
    std::string line;
    while (std::getline(in_dna, line)) {
        for (char& c : line) {
            if (c == 'A')
                A += 1;
            else if (c == 'C')
                C += 1;
            else if (c == 'G')
                G += 1;
            else if (c == 'T')
                T += 1;
        }
    }

    return DnaBaseCount{ A, C, T, G };
}

void DNA(std::istream& in_dna, std::ostream& out_to)
{
    auto count = DNA_count(in_dna);
    out_to << count.A << " " << count.C << " " << count.G << " " << count.T;
}
