#include "gc.hpp"
#include "common.hpp"
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <vector>

double gc_percent(std::string dna)
{
    int count = 0;
    for (char& c : dna) {
        if (c == 'G' || c == 'C')
            count += 1;
    }

    return static_cast<double>(count) / static_cast<double>(dna.length());
}

// The 'GC content' is the percentage of symbols that are 'G' and 'C'
void GC(std::istream& input, std::ostream& output)
{
    auto strings = FASTA(input);
    auto highest_gc_string = std::max_element(
        strings.begin(), strings.end(),
        [](const DnaString& dna_str_left, const DnaString& dna_str_right) {
            return gc_percent(dna_str_left.content) < gc_percent(dna_str_right.content);
        });

    output << highest_gc_string->label << std::endl
           << std::setprecision(9) << 100.0 * gc_percent(highest_gc_string->content);
}
