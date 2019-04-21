#include "cons.hpp"
#include "common.hpp"
#include <vector>

void CONS(std::istream& input, std::ostream& output)
{
    std::vector<DnaString> dnas = FASTA(input);
    auto dnaSize = dnas[0].content.length();

    std::vector<int> A_counts(dnaSize, 0);
    std::vector<int> C_counts(dnaSize, 0);
    std::vector<int> G_counts(dnaSize, 0);
    std::vector<int> T_counts(dnaSize, 0);
    unsigned long position{ 0 };
    for (auto dna : dnas) {
        for (auto c : dna.content) {
            A_counts[position] += (c == 'A' ? 1 : 0);
            C_counts[position] += (c == 'C' ? 1 : 0);
            G_counts[position] += (c == 'G' ? 1 : 0);
            T_counts[position] += (c == 'T' ? 1 : 0);
            position += 1;
        }

        position = 0;
    }

    // compose the most common
    std::vector<char> consensus{};
    for (auto a_it = A_counts.begin(), c_it = C_counts.begin(), g_it = G_counts.begin(), t_it = T_counts.begin();
         a_it < A_counts.end() && c_it < C_counts.end() && g_it < G_counts.end() && t_it < T_counts.end();
         ++a_it, ++c_it, ++g_it, ++t_it) {

        auto max = *a_it;
        char maxC = 'A';
        if (*c_it > max) {
            max = *c_it;
            maxC = 'C';
        }
        if (*g_it > max) {
            max = *g_it;
            maxC = 'G';
        }
        if (*t_it > max) {
            max = *t_it;
            maxC = 'T';
        }

        consensus.push_back(maxC);
    }

    output << std::string(consensus.begin(), consensus.end());
    output << std::endl
           << "A:";
    for (auto cnt : A_counts)
        output << " " << cnt;
    output << std::endl
           << "C:";
    for (auto cnt : C_counts)
        output << " " << cnt;
    output << std::endl
           << "G:";
    for (auto cnt : G_counts)
        output << " " << cnt;
    output << std::endl
           << "T:";
    for (auto cnt : T_counts)
        output << " " << cnt;
}
