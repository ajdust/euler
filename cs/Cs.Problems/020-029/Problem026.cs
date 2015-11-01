// Author: Aaron Johnson
// Date:   2015-02-28
//
// Solves Euler Problem 26.

using System;
using System.Collections.Generic;
using System.Linq;

namespace euler
{
    /// <title>Reciprocal cycles</title>
    /// <summary>
    /// A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:
    ///
    ///     1/2	= 	0.5
    ///     1/3	= 	0.(3)
    ///     1/4	= 	0.25
    ///     1/5	= 	0.2
    ///     1/6	= 	0.1(6)
    ///     1/7	= 	0.(142857)
    ///     1/8	= 	0.125
    ///     1/9	= 	0.(1)
    ///     1/10	= 	0.1
    ///
    /// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
    ///
    /// Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
    /// </summary>
    /// <answer>983</answer>
    public class Problem026 : IProblem
    {
        /// <summary>
        /// Infinite sequence. Calculates each digit and the paired long-division
        /// numerator that it came from. Repeating cycles in decimal fractions can be
        /// found by using this numerator.
        /// </summary>
        public IEnumerable<Tuple<int, int>> ManualDivision(int numerator, int denominator)
        {
            // First step - find first fractional digit, first subtractee and subtractor
            var lastDigit = numerator / denominator;
            yield return Tuple.Create(lastDigit, numerator);

            while (true)
            {
                numerator = (numerator - lastDigit * denominator) * 10;
                lastDigit = numerator / denominator;
                yield return Tuple.Create(lastDigit, numerator);
            }
        }

        public int CalculateCycleLength(int numerator, int denominator)
        {
            var numerators = new List<int>();
            foreach (var step in ManualDivision(numerator, denominator))
            {
                if (numerators.Contains(step.Item2))
                {
                    return numerators.Count() - numerators.IndexOf(step.Item2);
                }
                else
                {
                    numerators.Add(step.Item2);
                }
            }

            return -1;
        }

        public string Solve()
        {
            return Enumerable.Range(2, 998)
                .Select(n => Tuple.Create(n, CalculateCycleLength(1, n)))
                .Aggregate((a, b) => a.Item2 > b.Item2 ? a : b).Item1.ToString();
        }
    }
}