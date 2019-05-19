using System;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Powerful digit sum</title>
    /// <summary>
    /// A googol (10**100) is a massive number: one followed by one-hundred zeros; 100**100 is
    /// almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum
    /// of the digits in each number is only 1.
    ///
    /// Considering natural numbers of the form, a** b, where a, b < 100, what is the maximum
    /// digital sum?
    /// </summary>
    public class Problem056 : IProblem
    {
        public string Solve()
        {
            var sums = from a in Enumerable.Range(1, 99).Select(x => new BigInteger(x))
                       from b in Enumerable.Range(1, 99)
                       select BigInteger.Pow(a, b).ToString().ToCharArray().Select(x => (int)char.GetNumericValue(x)).Sum();

            return sums.Max().ToString();
        }
    }
}
