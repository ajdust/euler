// Author: Aaron Johnson
// Date:   2015-02-13
//
// Solves Euler Problem 20.

using System;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Factorial digit sum</title>
    /// <summary>
    /// n! means n × (n − 1) × ... × 3 × 2 × 1
    /// For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
    /// and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
    /// Find the sum of the digits in the number 100!
    /// </summary>
    /// <answer>648</answer>
    public class Problem020 : IProblem
    {
        public string Solve()
        {
            return Enumerable.Range(1, 100).Aggregate(new BigInteger(1), (prev, curr) => prev * curr)
                .ToString().Select(Char.GetNumericValue).Sum().ToString();
        }
    }
}
