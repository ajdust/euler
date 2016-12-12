using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Prime Digit Replacements</title>
    /// <summary>
    /// By replacing the 1st digit of *3, it turns out that six of the nine possible values:
    ///     13, 23, 43, 53, 73, and 83, are all prime.
    /// 
    /// By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is
    /// the first example having seven primes among the ten generated numbers, yielding the family:
    ///     56003, 56113, 56333, 56443, 56663, 56773, and 56993.
    /// 
    /// Consequently 56003, being the first member of this family, is the smallest prime with this
    /// property.
    /// 
    /// Find the smallest prime which, by replacing part of the number (not necessarily adjacent
    /// digits) with the same digit, is part of an eight prime value family.
    /// </summary>
    public class Problem051 : IProblem
    {
        private IDictionary<int, IEnumerable<int[]>> _memoIndexes = new Dictionary<int, IEnumerable<int[]>>();

        // A way to obtain all possible 'replacement positions' for a certain length
        private IEnumerable<int[]> GetReplacementIndexes(int length)
        {
            if (_memoIndexes.ContainsKey(length))
                return _memoIndexes[length];

            var indexesList = new List<int[]>();
            for (var size = 1; size <= length; size++)
            {
                var positions = new char[length];
                for (var i = 0; i < length; i++)
                    positions[i] = i < size ? '1' : '0';
                Array.Sort(positions);

                foreach (var combo in Combinatorics.PermuteForward(positions))
                {
                    indexesList.Add(combo
                        .Select((x, i) => x == '1' ? i : -1)
                        .Where(x => x >= 0)
                        .ToArray());
                }
            }

            _memoIndexes[length] = indexesList;
            return indexesList;
        }

        private IEnumerable<int[]> GetReplacementIndexesForPrime(int prime)
        {
            return prime.ToString().ToCharArray()
                .Select((x, i) => Tuple.Create(x, i))
                .GroupBy(x => x.Item1)
                .Select(x => x.Select(y => y.Item2).ToArray());
        }

        private char[] _digits = new[] { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };
        private HashSet<int> _primes;

        private int[] TestReplacementIndexes(int prime)
        {
            var results = GetReplacementIndexesForPrime(prime)
                .Select(indxs =>
                    _digits.Select(d =>
                    {
                        // copy prime characters and set digits
                        var primeChars = prime.ToString().ToCharArray();
                        for (var i = 0; i < indxs.Length; i++)
                            primeChars[indxs[i]] = d;

                        if (primeChars[0] == '0')
                            return 0;

                        var newprime = int.Parse(new string(primeChars));
                        return _primes.Contains(newprime) ? newprime : 0;
                    }).Where(x => x > 0).ToArray()
                )
                .Where(x => x.Length > 1);

            return results.OrderByDescending(x => x.Length).FirstOrDefault() ?? new int[] { };
        }

        public string Solve()
        {
            _primes = new HashSet<int>(new Primes(1000000).PrimeList());
            foreach (var prime in _primes)
            {
                var test = TestReplacementIndexes(prime);
                if (test != null && test.Length == 8)
                    return test.Min().ToString();
            }

            return null;
        }
    }
}
