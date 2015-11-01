// Author: Aaron Johnson
// Date:   2015-02-10
// 
// Solves Euler Problem 14.

using System;
using System.Linq;

namespace euler
{
    /// <title>Longest Collatz sequence</title>
    /// <summary> 
    /// The following iterative sequence is defined for the set of positive integers:
    /// 
    /// n → n/2 (n is even)
    /// n → 3n + 1 (n is odd)
    /// 
    /// Using the rule above and starting with 13, we generate the following sequence:
    /// 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
    /// 
    /// It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting  /// numbers finish at 1.
    /// 
    /// Which starting number, under one million, produces the longest chain?
    /// 
    /// NOTE: Once the chain starts the terms are allowed to go above one million.
    /// </summary>
    /// <answer>837799</answer>
    public class Problem014 : IProblem
    {
        public string Solve()
        {
            return Enumerable.Range(2, 999998)
                .Select(n => new Tuple<long, long>(n, Sequences.Collatz(n).Count()))
                .Aggregate(new Tuple<long, long>(0, 0), (prev, next) => next.Item2 > prev.Item2 ? next : prev)
                .Item1.ToString();
        }
    }
}
