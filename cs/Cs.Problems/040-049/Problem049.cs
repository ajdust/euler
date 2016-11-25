using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Three Prime Permutations</title>
    /// <summary>
    /// The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
    /// (i) each of the three terms are prime, and,
    /// (ii) each of the 4-digit numbers are permutations of one another.
    ///
    /// There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there
    /// is one other 4-digit increasing sequence.
    ///
    /// What 12-digit number do you form by concatenating the three terms in this sequence?
    /// </summary>
    public class Problem049 : IProblem
    {
        public string Solve()
        {
            var primes = new HashSet<int>(new Primes(10000).PrimeList().Where(x => x > 1000));

            var matching = new List<int>();

            foreach (var prime in primes)
            {
                if (matching.Contains(prime))
                    continue;

                var ps = Combinatorics.PermuteForward(prime.ToString().ToArray())
                    .Select(y => int.Parse(new string(y)))
                    .Where(y => primes.Contains(y))
                    .ToList();

                ps.Sort();
                if (ps.Count() >= 3 && !ps.Contains(1487) && ps[1] + (ps[1] - ps[0]) == ps[2])
                    matching.AddRange(ps);
            }

            return string.Join("", matching.OrderBy(x => x).Select(x => x.ToString()));
        }
    }
}
