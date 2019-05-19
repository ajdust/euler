using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Goldbach Composites</title>
    /// <summary>
    /// It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
    /// 
    /// 9 = 7 + 2×1^2
    /// 15 = 7 + 2×2^2
    /// 21 = 3 + 2×3^2
    /// 25 = 7 + 2×3^2
    /// 27 = 19 + 2×2^2
    /// 33 = 31 + 2×1^2
    /// 
    /// It turns out that the conjecture was false.
    /// 
    /// What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
    /// </summary>
    public class Problem046 : IProblem
    {
        private Primes _primes = null;
        private IList<int> _primesList = new List<int>();
        private HashSet<int> _doubleSquares = new HashSet<int>() { 2 };
        private int _lastDoubleSquareAdded = 1;

        private int GetIndexOfLessThanOrEqual(IList<int> sortedList, int n)
        {
            var lower = 0;
            var upper = sortedList.Count;

            while (upper - lower > 1)
            {
                var mid = lower + (upper - lower) / 2;
                var current = sortedList[mid];

                if (current == n) return mid;
                // look higher
                else if (current < n) lower = mid;
                // look lower
                else upper = mid;
            }

            while (upper > 0 && sortedList[upper] > n)
                upper--;

            return upper;
        }

        private IEnumerable<int> Forever(int i, int step)
        {
            while (true)
            {
                i += step;
                yield return i;
            }
        }

        private bool DoubleSquareContains(int i)
        {
            // add more squares if needed
            if (_doubleSquares.Max() < i)
            {
                _doubleSquares.UnionWith(
                    Enumerable.Range(_lastDoubleSquareAdded, 100).Select(x => 2 * x * x));
            }

            return _doubleSquares.Contains(i);
        }

        private bool NotPrime(int n)
        {
            return PrimesUnder(n).Last() != n;
        }

        private IEnumerable<int> PrimesUnder(int n)
        {
            // add more primes if needed
            if (_primesList.LastOrDefault() < n) checked
            {
                _primes = new Primes(n * n > 100000 ? n * n : 100000);
                _primesList = _primes.PrimeList().ToList();
            }

            var i = GetIndexOfLessThanOrEqual(_primesList, n);
            return _primesList.Take(i + 1);
        }

        private long BruteSolve()
        {
            return Forever(15, 2)
                .Where(x => NotPrime(x))
                .First(i => !PrimesUnder(i).Any(p => DoubleSquareContains(i - p)));
        }

        public string Solve()
        {
            return BruteSolve().ToString();
        }
    }
}
