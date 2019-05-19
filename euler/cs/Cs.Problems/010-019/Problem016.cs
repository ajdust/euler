// Author: Aaron Johnson
// Date: 2015-02-10
//
// Solves Euler Problem 16.

using System;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Power digit sum</title>
    /// <summary>
    /// 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
    /// What is the sum of the digits of the number 21000?
    /// </summary>
    /// <remarks>With a decent arbitrarily big number representation, this problem is almost too simple.</remarks>
    /// <answer>1366</answer>
    public class Problem016 : IProblem
    {
        public string Solve()
        {
            var n = BigInteger.Pow(new BigInteger(2), 1000);
            return n.ToString().ToCharArray().Select(Char.GetNumericValue).Sum().ToString();
            // in Python: sum(int(x) for x in str(2**1000))
        }
    }
}
