#include <iostream>

struct DnaBaseCount {
    int A, C, T, G;
};

DnaBaseCount DNA_count(std::istream& in_dna);
void DNA(std::istream& in_dna, std::ostream& out_to);
