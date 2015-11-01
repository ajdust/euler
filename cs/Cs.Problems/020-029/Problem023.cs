// Author: Aaron Johnson
// Date:   2015-02-22
//
// Solves Euler Problem 23.

using System.Collections.Generic;
using System.Linq;

namespace euler
{
    /// <title>Non-abundant sums</title>
    /// <summary>
    /// A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
    /// For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
    /// 
    /// A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
    /// 
    /// As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant 
    /// numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two 
    /// abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest 
    /// number that cannot be expressed as the sum of two abundant numbers is less than this limit.
    /// 
    /// Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
    /// </summary>
    /// <remarks>This problem uses Problem 12's GetFactors and Problem 3's LongRange </remarks>
    /// <answer>4179871</answer>
    public class Problem023 : IProblem 
    {

        public string Solve()
        {
            var abundantSums = new HashSet<long>(Sequences.GetAbundantSums(28123).ToList());
            var answer = Enumerable.Range(1, 28123).Where(n => !abundantSums.Contains(n)).Sum();
            return answer.ToString();
        }
    }
}
