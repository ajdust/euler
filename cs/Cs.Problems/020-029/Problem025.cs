// Author: Aaron Johnson
// Date:   2015-02-22
// 
// Solves Euler Problem 25.

using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>1000-digit Fibonacci number</title>
    /// <summary>
    /// The Fibonacci sequence is defined by the recurrence relation:
    /// F_n = F_n−1 + F_n−2, where F_1 = 1 and F_2 = 1.
    /// What is the first term in the Fibonacci sequence to contain 1000 digits?
    /// </summary>
    /// <remarks>This solution uses Problem 17's Enumerate.</remarks>
    /// <answer>4782</answer>
    public class Problem025 : IProblem
    {
        public string Solve()
        {
            var answer = Sequences.BigFibonacci().Enumerate().First(t => t.Item2 > BigInteger.Pow(10, 999));
            return answer.Item1.ToString();
        }
    }
}
