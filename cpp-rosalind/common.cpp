#include "common.hpp"
#include <istream>
#include <string>
#include <vector>

std::vector<DnaString> FASTA(std::istream& in_fasta)
{
    std::vector<DnaString> strings{};
    std::string line;
    DnaString dna_str{};

    // get the first entry
    if (!std::getline(in_fasta, line)
        || line.at(0) != '>')
        return strings;

    dna_str.label = line.substr(1, line.length());

    while (std::getline(in_fasta, line)) {
        if (line.length() == 0) {
            continue;
        } else if (line.at(0) == '>') {
            strings.push_back(dna_str);
            dna_str = DnaString{};
            dna_str.label = line.substr(1, line.length());
        } else {
            dna_str.content += line;
        }
    }

    strings.push_back(dna_str);
    return strings;
}
