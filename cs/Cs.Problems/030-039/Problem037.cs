using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <summary>
    /// The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. /// Similarly we can work from right to left: 3797, 379, 37, and 3.
    /// 
    /// Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
    /// 
    /// NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
    /// </summary>
    public class Problem037 : IProblem
    {
        private static char[] leftmost = new[] { '2', '3', '5', '7' };
        private static char[] rightmost = new[] { '3', '7' };
        private static char[] inner = new[] { '1', '3', '7', '9' };

        /// <summary>
        /// To be truncatable from both the left and the right, a few rules must hold:
        /// - The leftmost digit can only be 2, 3, 5, or 7
        /// - The right digit can only be 3 or 7 (no number above 10 ending in 2 or 5 is prime)
        /// - Similarly, each digit between right and left could only be 1, 3, or 7, 9
        /// </summary>
        private bool IsTruncatableFirstPass(int prime)
        {
            var s = prime.ToString();
            if (s.Length < 2)
                return false;

            return leftmost.Contains(s[0])
                && rightmost.Contains(s[s.Length - 1])
                && s.Substring(1, s.Length - 2).All(x => inner.Contains(x));
        }

        private List<int> Truncations(int number)
        {
            var s = number.ToString();
            var truncations = new List<int>(s.Length * 2) { number };
            for (var i = 1; i < s.Length; i++)
            {
                truncations.Add(Convert.ToInt32(s.Substring(i)));
                truncations.Add(Convert.ToInt32(s.Substring(0, i+1)));
            }

            return truncations;
        }

        public IEnumerable<int> BruteSolve()
        {
            var primeCalculator = new Primes(1000000);
            var primes = new HashSet<int>(primeCalculator.PrimeList());
            return primes
                .Where(IsTruncatableFirstPass)
                .Where(x => Truncations(x).All(y => primes.Contains(y)));
        }

        public string Solve()
        {
            return BruteSolve().Sum().ToString();
        }
    }
}
