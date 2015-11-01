using System;
using System.Collections.Generic;
using System.Linq;

namespace euler
{
    /// <title>Smallest multiple</title>
    /// <summary>
    /// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    /// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
    /// </summary>
    /// <remarks>
    /// This problem builds off of problem 3, Largest prime factor, which provided a method to list the 
    /// prime factors of a number.
    /// </remarks>
    /// <answer>232792560</answer>
    public class Problem005 : IProblem
    {
        static long CommonFactorCountSolve()
        {
            // find the common factors between the numbers 1 to 20
            var factorCounts = Sequences.LongRange(1, 20).Select(n => Sequences.GetPrimeFactors(n).GroupBy(x => x).ToDictionary(g => g.Key, g => g.Count()));

            // collect the maximum factor counts for each of the numbers;
            // for instance, 16 is 2^4, so the 2 factors of 20 (2*2*5)
            // are not necessary as they are already accounted for
            var maxCount = new Dictionary<long, int>();
            foreach (var factor in factorCounts.SelectMany(factorCount => factorCount))
            {
                maxCount[factor.Key] = maxCount.ContainsKey(factor.Key)
                    ? Math.Max(maxCount[factor.Key], factor.Value)
                    : factor.Value;
            }

            // compute the smallest number that is divisible by all numbers from 1 to 20,
            // ie, the number with the minimum number of factors shared by those numbers
            return maxCount.Aggregate<KeyValuePair<long, int>, long>(
                1, (current, factor) => current * (long)Math.Pow(factor.Key, factor.Value)
            );
        }

        public string Solve()
        {
            return CommonFactorCountSolve().ToString();
        }
    }
}
