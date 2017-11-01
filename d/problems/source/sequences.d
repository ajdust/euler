module sequences;

import std.range;
import std.algorithm;
import std.functional;

auto fibonacci(T)(T a, T b)
{
    T a_ = a;
    T b_ = b;
    T temp = a;
    return () { temp = a_ + b_; a_ = b_; b_ = temp; return a_; };
}

auto primes(T)()
{
    T n = 3;
    T last = 2;
    T[T] sieve;
    return () {
        auto prime = sieve.get(n, 0);
        while (prime != 0)
        {
            sieve.remove(n);
            auto n_ = n + prime + prime;
            while (n_ in sieve)
            {
                n_ += prime + prime;
            }
            sieve[n_] = prime;
            n += 2;
            prime = sieve.get(n, 0);
        }

        sieve[n * n] = n;
        auto t = last;
        last = n;
        n += 2;
        return t;
    };
}

T[] primeFactors(T)(T of)
{
    auto primes = generate(primes!T);
    T[] pfacts;
    T quotient = of;

    foreach (T prime; primes)
    {
        if (prime > quotient)
        {
            break;
        }

        auto remainder = quotient % prime;
        while (remainder == 0)
        {
            quotient = quotient / prime;
            remainder = quotient % prime;
            pfacts ~= prime;
        }
    }

    return pfacts;
}

auto triangeNumbers(T)()
{
    auto currentN = 1;
    auto currentTotal = 0;
    return () {
        currentTotal += currentN;
        currentN += 1;
        return currentTotal;
    };
}

auto distinct(T)(T[] ts)
{
    return ts.sort().uniq();
}