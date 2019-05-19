using System;
using System.Collections.Generic;

namespace factorbench
{
    class PrimeGenerator
    {
        long n = 3;
        long last = 2;
        Dictionary<long, long> sieve = new Dictionary<long, long>();

        public long Next()
        {
            while (sieve.TryGetValue(n, out var prime))
            {
                sieve.Remove(n);

                var composite = n + prime + prime;
                while (sieve.ContainsKey(composite))
                    composite += prime + prime;
                sieve.Add(composite, prime);

                n = n + 2;
            }

            sieve.Add(n * n, n);
            var r = last;
            last = n;
            n += 2;
            return r;
        }
    }

    class FactorFinder
    {
        Dictionary<long, HashSet<long>> known = new Dictionary<long, HashSet<long>>();
        PrimeGenerator nextPrimes = new PrimeGenerator();
        List<long> knownPrimes = new List<long>();

        public FactorFinder()
        {
            known.Add(1, new HashSet<long> { 1 });
        }

        public List<long> GetPrimeFactors(long of)
        {
            var factors = new List<long>();
            var quotient = of;

            foreach (var prime in knownPrimes)
            {
                if (prime > quotient)
                    return factors;

                var remainder = quotient % prime;
                while (remainder == 0)
                {
                    quotient /= prime;
                    remainder = quotient % prime;
                    factors.Add(prime);
                }
            }

            for (;;)
            {
                var prime = nextPrimes.Next();
                knownPrimes.Add(prime);

                if (prime > quotient)
                    return factors;

                var remainder = quotient % prime;
                while (remainder == 0)
                {
                    quotient /= prime;
                    remainder = quotient % prime;
                    factors.Add(prime);
                }
            }
        }

        public HashSet<long> GetFactors(long of)
        {
            if (known.TryGetValue(of, out var factors))
                return factors;

            factors = new HashSet<long> { 1, of };

            foreach (var prime in GetPrimeFactors(of))
            {
                var factor = of / prime;
                foreach (var subfactor in GetFactors(factor))
                {
                    factors.Add(subfactor);
                }
            }

            known.Add(of, factors);
            return factors;
        }
    }

    static class MainProgram
    {
        static long Solve()
        {
            var finder = new FactorFinder();
            long adder = 0;
            long tn = 0;

            for (;;)
            {
                adder += 1;
                tn += adder;
                var factors = finder.GetFactors(tn);
                if (factors.Count > 1000)
                    return tn;
            }
        }

        public static void Main(string[] args)
        {
            var answer = Solve();
            Console.WriteLine($"Answer: {answer}");
        }
    }
}