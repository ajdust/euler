module factorbench;

import std.stdio;
import std.conv;

alias setlong = bool[long];

void insert(setlong s, long v)
{
    s[v] = true;
}

class PrimeGenerator
{
private:
    long n = 3;
    long last = 2;
    long[long] sieve;
public:
    long next()
    {
        auto it = (n in sieve);
        while (it !is null) {

            auto prime = *it;
            sieve.remove(prime);

            long composite = n + prime + prime;
            while (composite in sieve) {
                composite += prime + prime;
            }
            sieve[composite] = prime;
            n += 2;

            it = (n in sieve);
        }

        sieve[n * n] = n;
        long r = last;
        last = n;
        n += 2;
        return r;
    }
}


class FactorFinder
{
private:
    setlong[long] known;
    PrimeGenerator next_primes;
    long[] known_primes;

public:
    this()
    {
        next_primes = new PrimeGenerator();
        known[1] = [ 1: true ];
    }

    long[] get_prime_factors(long of)
    {
        long[] factors;
        auto quotient = of;

        foreach (long prime; known_primes)
        {
            if (prime > quotient)
                return factors;

            auto remainder = quotient % prime;
            while (remainder == 0)
            {
                quotient = quotient / prime;
                remainder = quotient % prime;
                factors ~= prime;
            }
        }

        for (;;)
        {
            auto prime = next_primes.next();
            known_primes ~= prime;

            if (prime > quotient)
                return factors;

            auto remainder = quotient % prime;
            while (remainder == 0)
            {
                quotient = quotient / prime;
                remainder = quotient % prime;
                factors ~= prime;
            }
        }
    }

    setlong get_factors(long of)
    {
        auto existing = (of in known);
        if (existing !is null)
            return *existing;

        auto pfactors = get_prime_factors(of);
        setlong factor_set = [ 1: true, of: true ];

        foreach (long prime; pfactors)
        {
            auto factor = of / prime;
            foreach (long subfactor; get_factors(factor).byKey())
            {
                factor_set.insert(subfactor);
            }
        }

        known[of] = factor_set;
        return factor_set;
    }
}

long solve()
{
    FactorFinder ff = new FactorFinder();

    long adder = 0;
    long tn = 0;

    for (;;)
    {
        adder += 1;
        tn += adder;

        auto factors = ff.get_factors(tn);
        if (factors.length > 1000)
            return tn;
    }
}

int main()
{
    writeln("Answer: " ~ to!string(solve()) ~ "\n");
    return 0;
}