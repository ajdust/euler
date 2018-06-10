// #define EASTL_CORE_ALLOCATOR_ENABLED true
// #include <EASTL/core_allocator.h>

#include <iostream>
#include <EASTL/vector.h>
#include <EASTL/hash_map.h>
#include <EASTL/hash_set.h>

// This would undoubtedly run faster if I learned about memory allocators and used a custom one.

// Note:
// EASTL requires you to have an overload for the operator new[]
// here we forward to global new[]
// See EASTL_Project_Integration.md
void* operator new[](size_t size, const char* name, int flags, unsigned debugFlags, const char* file, int line)
{
    return new uint8_t[size];
    // std::cerr << "Unexpected call to global new operator.  Does EASTL have an ICoreAllocator set?\n";
    // std::terminate();
    // return nullptr;
}

void* operator new[](size_t size, size_t s2, size_t s3, const char* name, int flags, unsigned debugFlags, const char* file, int line)
{
    return new uint8_t[size];
    // std::cerr << "Unexpected call to global new operator.  Does EASTL have an ICoreAllocator set?\n";
    // std::terminate();
    // return nullptr;
}

namespace factorbench {

class PrimeGenerator {
private:
    long n;
    long last;
    eastl::hash_map<long, long> sieve;
public:

    PrimeGenerator() {
        n = 3;
        last = 2;
        sieve = eastl::hash_map<long, long>();
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
    eastl::hash_map<long, eastl::hash_set<long> > known;
    PrimeGenerator next_primes;
    eastl::vector<long> known_primes;
public:

    FactorFinder() {
        eastl::hash_set<long> one;
        one.insert(1);
        eastl::pair<long, eastl::hash_set<long> > justone(1, one);
        known.insert(justone);
    }

    eastl::vector<long> get_prime_factors(long of) {

        eastl::vector<long> factors;
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

    eastl::hash_set<long> get_factors(long of) {

        auto existing = known.find(of);
        if (existing != known.end()) {
            return existing->second;
        }

        auto pfactors = get_prime_factors(of);
        eastl::hash_set<long> factor_set;
        factor_set.insert(1);
        factor_set.insert(of);

        for (auto prime : pfactors) {

            auto factor = of / prime;
            for (auto subfactor : get_factors(factor)) {
                factor_set.insert(subfactor);
            }

        }

        known.insert(eastl::pair<long, eastl::hash_set<long> >(of, factor_set));
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

