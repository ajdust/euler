#include <cstdint>
#include <iostream>
#include <sparsehash/dense_hash_map>
#include <sparsehash/dense_hash_set>
#include <vector>

using google::dense_hash_set;
using google::dense_hash_map;

namespace factorbench {

class PrimeGenerator {
private:
    int64_t n = 3;
    int64_t last = 2;
    dense_hash_map<int64_t, int64_t> sieve;
public:
    PrimeGenerator() {
        sieve.set_empty_key(0);
    }

    int64_t next() {
        auto len = sieve.count(n);
        while (len > 0) {

            auto it = sieve[n];
            sieve.erase(it);
            auto prime = it;

            int64_t composite = n + prime + prime;
            while (sieve.count(composite) > 0) {
                composite += prime + prime;
            }
            sieve[composite] = prime;
            n += 2;

            len = sieve.count(n);
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
    dense_hash_map<int64_t, dense_hash_set<int64_t>> known;
    PrimeGenerator next_primes;
    std::vector<int64_t> known_primes;
public:

    FactorFinder() {
        dense_hash_set<int64_t> initSet;
        initSet.set_empty_key(0);
        initSet.insert(1);
        known.set_empty_key(0);
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
                factors.emplace_back(prime);
            }
        }

        for (;;) {
            auto prime = next_primes.next();
            known_primes.emplace_back(prime);

            if (prime > quotient) {
                return factors;
            }

            auto remainder = quotient % prime;
            while (remainder == 0) {
                quotient = quotient / prime;
                remainder = quotient % prime;
                factors.emplace_back(prime);
            }
        }

        return factors;
    }

    dense_hash_set<int64_t> get_factors(int64_t of) {

        auto existing = known.find(of);
        if (existing != known.end()) {
            return existing->second;
        }

        auto pfactors = get_prime_factors(of);
        dense_hash_set<int64_t> factor_set;
        factor_set.set_empty_key(0);
        factor_set.insert(1);
        factor_set.insert(of);

        for (auto prime : pfactors) {

            auto factor = of / prime;
            for (auto subfactor : get_factors(factor)) {
                factor_set.insert(subfactor);
            }

        }

        known.insert({ of, factor_set });
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

