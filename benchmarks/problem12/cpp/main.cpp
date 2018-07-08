#include <cstdint>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace factorbench {

class PrimeGenerator {
private:
    int64_t n = 3;
    int64_t last = 2;
    std::unordered_map<int64_t, int64_t> sieve = {};
public:
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
    std::unordered_map<int64_t, std::unordered_set<int64_t>> known;
    PrimeGenerator next_primes;
    std::vector<int64_t> known_primes;
public:

    FactorFinder() {
        known.insert({ 1, { 1 } });
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

    std::unordered_set<int64_t> get_factors(int64_t of) {

        auto existing = known.find(of);
        if (existing != known.end()) {
            return existing->second;
        }

        auto pfactors = get_prime_factors(of);
        std::unordered_set<int64_t> factor_set { 1, of };

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

