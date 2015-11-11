// Author: Aaron Johnson
// Date:   2015-03-04
//
// Solves Euler Problem 27.

using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Quadratic primes</title>
    /// <summary>
    /// Euler discovered the remarkable quadratic formula:
    ///
    /// n² + n + 41
    ///
    /// It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39.
    /// However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when
    /// n = 41, 41² + 41 + 41 is clearly divisible by 41.
    ///
    /// The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the
    /// consecutive values n = 0 to 79. The product of the coefficients, −79 and 1601, is −126479.
    ///
    /// Considering quadratics of the form:
    ///
    ///     n² + an + b, where |a| < 1000 and |b| < 1000
    ///
    ///     where |n| is the modulus/absolute value of n
    ///     e.g. |11| = 11 and |−4| = 4
    ///
    /// Find the product of the coefficients, a and b, for the quadratic expression that produces
    /// the maximum number of primes for consecutive values of n, starting with n = 0.
    ///
    /// </summary>
    /// <remarks>This problem uses Problem 10's Primes class.</remarks>
    /// <answer>-59231</answer>
    public class Problem027 : IProblem
    {
        private IEnumerable<int> QuadraticWith(int a, int b)
        {
            var n = 0;
            for (; ; n++)
            {
                // n^2 + an + b = n(n + a) + b
                yield return n * (n + a) + b;
            }
        }

        public Tuple<int, int, int> FindPrimeFormula()
        {
            // one of the largest possible value is 1000*1000 + 1000*1000 + 1000 < 2100000
            // with the restrictions given
            var primes = new Primes(2100000);
            var primesSet = new HashSet<int>(primes.PrimeList());

            var abCombinations = from i in Enumerable.Range(-1000, 2000)
                                 from j in Enumerable.Range(-1000, 2000)
                                 select Tuple.Create(i, j);

            var answer = abCombinations.Select(n =>
            {
                var count = QuadraticWith(n.Item1, n.Item2).TakeWhile(primesSet.Contains).Count();
                return Tuple.Create(n.Item1, n.Item2, count);
            }).Aggregate((a, b) => a.Item3 > b.Item3 ? a : b);

            return answer;
        }

        public string Solve()
        {
            var answer = FindPrimeFormula();
            return (answer.Item1 * answer.Item2).ToString();
        }
    }
}