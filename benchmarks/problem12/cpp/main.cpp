#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>

namespace factorbench {

class PrimeGenerator {
private:
    long n;
    long last;
    std::unordered_map<long, long> sieve;
public:

    PrimeGenerator() {
        n = 3;
        last = 2;
        sieve = std::unordered_map<long, long>();
    }

    long next() {
        auto it = sieve.find(n);
        while (it != sieve.end()) {

            sieve.erase(it);
            auto prime = it->second;

            long composite = n + prime + prime;
            while (sieve.find(composite) != sieve.end()) {
                composite += prime + prime;
            }
            sieve[composite] = prime;
            n += 2;

            it = sieve.find(n);
        }

        sieve[n * n] = n;
        long r = last;
        last = n;
        n += 2;
        return r;
    }
};


class FactorFinder {
private:
    std::unordered_map<long, std::unordered_set<long>> known;
    std::vector<long> known_primes;
    PrimeGenerator next_primes;
public:

    FactorFinder() {
        std::unordered_set<long> one;
        one.insert(1);
        std::pair<long, std::unordered_set<long>> justone(1, one);
        known.insert(justone);
    }

    std::vector<long> get_prime_factors(long of) {

        std::vector<long> factors;
        uint count_known_primes = known_primes.size();
        auto quotient = of;

        for (uint i = 0; i < count_known_primes; i++) {
            auto prime = known_primes.at(i);

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

    std::unordered_set<long> get_factors(long of) {

        auto existing = known.find(of);
        if (existing != known.end()) {
            return existing->second;
        }

        auto pfactors = get_prime_factors(of);
        std::unordered_set<long> factor_set(pfactors.begin(), pfactors.end());
        factor_set.insert(1);
        factor_set.insert(of);

        for (auto prime : pfactors) {

            auto factor = of / prime;
            for (auto subfactor : get_factors(factor)) {
                factor_set.insert(subfactor);
            }

        }

        known.insert(std::pair<long, std::unordered_set<long>>(of, factor_set));
        return factor_set;
    }
};


long solve() {

    FactorFinder pf;

    long adder = 0;
    long tn = 0;

    for (;;) {
        adder += 1;
        tn += adder;

        auto factors = pf.get_factors(tn);
        if (factors.size() > 1000) {
            return tn;
        }
    }
}

} // end of namespace

int main() {
    auto answer = factorbench::solve();
    std::cout << "Answer: " << answer << '\n';
    return 0;
}

