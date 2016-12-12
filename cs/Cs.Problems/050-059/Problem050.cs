using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Prime of Sum Primes</title>
    /// <summary>
    /// The prime 41, can be written as the sum of six consecutive primes:
    /// 
    /// 41 = 2 + 3 + 5 + 7 + 11 + 13
    /// 
    /// This is the longest sum of consecutive primes that adds to a prime below one-hundred.
    /// The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21
    /// terms, and is equal to 953.
    /// 
    /// Which prime, below one-million, can be written as the sum of the most consecutive primes?
    /// </summary>
    public class Problem050 : IProblem
    {
        public string Solve()
        {
            var primesList = new Primes(1000000).PrimeList().ToArray();
            var primes = new HashSet<int>(primesList);

            // brute force: check every combination of consecutive primes
            var largestStart = 0;
            var largestCount = 0;
            var largestPrime = 0;

            for (var i = 1; i < primesList.Length; i++)
            {
                var sum = primesList[i];
                for (var j = i + 1; j < primesList.Length; j++)
                {
                    sum += primesList[j];
                    if (sum > 1000000) break;

                    if (j - i > largestCount && primes.Contains(sum))
                    {
                        largestStart = i;
                        largestCount = j - i;
                        largestPrime = sum;
                    }
                }

                if (sum > 1000000) continue;
            }

            return largestPrime.ToString();
        }
    }
}
