using System;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Consecutive Integer Prime Factors</title>
    /// <summary>
    /// The first two consecutive numbers to have two distinct prime factors are:
    /// 
    /// 14 = 2 × 7
    /// 15 = 3 × 5
    /// 
    /// The first three consecutive numbers to have three distinct prime factors are:
    /// 
    /// 644 = 2² × 7 × 23
    /// 645 = 3 × 5 × 43
    /// 646 = 2 × 17 × 19.
    /// 
    /// Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?
    /// </summary>
    public class Problem047 : IProblem
    {
        public string Solve()
        {
            var answer = Sequences.LongRange(1, int.MaxValue)
                .First(n =>
                {
                    var count = new[] { n, n + 1, n + 2, n + 3 }
                        .TakeWhile(x => Sequences.GetPrimeFactors(x).Distinct().Count() == 4)
                        .Count();
                    return count == 4;
                });

            return answer.ToString();
        }
    }
}
