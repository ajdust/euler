#ifndef COMMON_HPP
#define COMMON_HPP
#include <vector>
#include <istream>

struct DnaString {
    std::string label;
    std::string content;
};

std::vector<DnaString> FASTA(std::istream& in_fasta);

#endif // COMMON_HPP
