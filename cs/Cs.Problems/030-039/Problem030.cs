// Author: Aaron Johnson
// Date:   2015-03-07
//
// Solves Euler problem 30.

using System;
using System.Linq;

namespace euler
{
    /// <title>Digit fifth powers</title>
    /// <summary>
    /// Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
    ///
    ///     1634 = 1^4 + 6^4 + 3^4 + 4^4
    ///     8208 = 8^4 + 2^4 + 0^4 + 8^4
    ///     9474 = 9^4 + 4^4 + 7^4 + 4^4
    ///
    /// As 1 = 14 is not a sum it is not included.
    ///
    /// The sum of these numbers is 1634 + 8208 + 9474 = 19316.
    ///
    /// Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
    ///
    /// </summary>
    /// <remarks>
    /// This can be solved in one line with Python as well:
    /// sum( [x for x in range(10, 6*9**5) if x == sum( int(y)**5 for y in str(x) )] )
    /// </remarks>
    /// <answer>443839</answer>
    public class Problem030 : IProblem
    {
        public string Solve()
        {
            return Enumerable.Range(10, (int)(6 * Math.Pow(9, 5)))
                .Where(x => x == x.ToString().ToCharArray().Select(Char.GetNumericValue)
                                  .Select(s => (int)Math.Pow(s, 5))
                                  .Sum())
                .Sum().ToString();
        }
    }
}