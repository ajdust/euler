#include <cstdint>
#include <iostream>
#include "robin_hood.h"
#include <vector>

using namespace robin_hood;

namespace factorbench {

class PrimeGenerator {
private:
    int64_t n = 3;
    int64_t last = 2;
    unordered_map<int64_t, int64_t> sieve;
public:
    PrimeGenerator() {}

    int64_t next() {
        while (sieve.find(n) != sieve.end()) {

            auto prime = sieve[n];
            sieve.erase(n);

            int64_t composite = n + prime + prime;
            while (sieve.find(composite) != sieve.end()) {
                composite += prime + prime;
            }
            sieve[composite] = prime;
            n += 2;
        }

        sieve[n * n] = n;
        int64_t r = last;
        last = n;
        n += 2;
        return r;
    }
};


class FactorFinder {
private:
    unordered_map<int64_t, unordered_set<int64_t>> known;
    PrimeGenerator next_primes;
    std::vector<int64_t> known_primes;
public:

    FactorFinder() {
        unordered_set<int64_t> initSet;

        initSet.insert(1);

        known[1] = initSet;
    }

    std::vector<int64_t> get_prime_factors(int64_t of) {

        std::vector<int64_t> factors;
        auto quotient = of;

        for (auto const& prime : known_primes) {
            if (prime > quotient) {
                return factors;
            }

            auto remainder = quotient % prime;
            while (remainder == 0) {
                quotient = quotient / prime;
                remainder = quotient % prime;
                factors.push_back(prime);
            }
        }

        for (;;) {
            auto prime = next_primes.next();
            known_primes.push_back(prime);

            if (prime > quotient) {
                return factors;
            }

            auto remainder = quotient % prime;
            while (remainder == 0) {
                quotient = quotient / prime;
                remainder = quotient % prime;
                factors.push_back(prime);
            }
        }

        return factors;
    }

    unordered_set<int64_t> get_factors(int64_t of) {

        if (known.find(of) != known.end()) {
            return known[of];
        }

        unordered_set<int64_t> factor_set;
        factor_set.insert(1);
        factor_set.insert(of);

        auto pfactors = get_prime_factors(of);
        for (auto prime : pfactors) {

            auto factor = of / prime;
            for (auto subfactor : get_factors(factor)) {
                factor_set.insert(subfactor);
            }

        }

        known[of] = factor_set;
        return factor_set;
    }
};


int64_t solve() {

    FactorFinder pf;

    int64_t adder = 0;
    int64_t tn = 0;

    for (;;) {
        adder += 1;
        tn += adder;

        auto factors = pf.get_factors(tn);
        if (factors.size() > 1000) {
            return tn;
        }
    }
}

} // namespace factorbench

int main() {
    auto answer = factorbench::solve();
    std::cout << "Answer: " << answer << '\n';
    return 0;
}

