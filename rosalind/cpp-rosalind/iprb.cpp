#include "iprb.hpp"
#include "string_util.hpp"
#include <vector>

enum SimpleMendel { Dominant,
    Recessive,
    Heterozygous };

// k homozygous dominant, n homozygous recessive, m heterozygous
double IPRB(int k, int m, int n)
{
    std::vector<SimpleMendel> population{};
    for (int i{ 0 }; i < k; ++i)
        population.push_back(Dominant);
    for (int i{ 0 }; i < m; ++i)
        population.push_back(Heterozygous);
    for (int i{ 0 }; i < n; ++i)
        population.push_back(Recessive);

    // count every type of combination of mates
    int dom_dom_pair{ 0 };
    int dom_het_pair{ 0 };
    int dom_rec_pair{ 0 };
    int het_het_pair{ 0 };
    int het_rec_pair{ 0 };
    int rec_rec_pair{ 0 };
    int total_count{ 0 };

    for (unsigned long i{ 0 }; i < population.size(); ++i) {
        for (unsigned long j{ i }; j < population.size(); ++j) {
            // cannot mate with yourself
            if (j == i)
                continue;

            total_count += 1;
            auto m1{ population[i] };
            auto m2{ population[j] };
            if (m1 == Dominant) {
                if (m2 == Dominant)
                    dom_dom_pair += 1;
                else if (m2 == Heterozygous)
                    dom_het_pair += 1;
                else if (m2 == Recessive)
                    dom_rec_pair += 1;
            } else if (m1 == Heterozygous) {
                if (m2 == Dominant)
                    dom_het_pair += 1;
                else if (m2 == Heterozygous)
                    het_het_pair += 1;
                else if (m2 == Recessive)
                    het_rec_pair += 1;
            } else if (m1 == Recessive) {
                if (m2 == Dominant)
                    dom_rec_pair += 1;
                else if (m2 == Heterozygous)
                    het_rec_pair += 1;
                else if (m2 == Recessive)
                    rec_rec_pair += 1;
            }
        }
    }

    return ( // always results in dominant
               (dom_dom_pair + dom_het_pair + dom_rec_pair) +
               // results in dominant 75% of the time
               (het_het_pair * 0.75) +
               // results in dominant 50% of the time
               (het_rec_pair * 0.50))
        / total_count;
}

void IPRB(std::istream& input, std::ostream& output)
{
    std::string line{};
    std::getline(input, line);
    std::vector<int> args{};
    for (auto s : split_space(line)) {
        args.push_back(std::stoi(s));
    }

    int k{ args[0] };
    int m{ args[1] };
    int n{ args[2] };
    output << "k: " << k
           << " m: " << m
           << " n: " << n
           << " iprb: " << IPRB(k, m, n);
}
