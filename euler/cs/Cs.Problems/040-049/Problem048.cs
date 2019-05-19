using System;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Last Digits of Powers Sequence</title>
    /// <summary>
    /// The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
    /// Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
    /// </summary>
    public class Problem048 : IProblem
    {
        public string Solve()
        {
            var limit = new BigInteger(1e10);
            var sum = Enumerable.Range(1, 1000)
                // remove divisible by 10
                .Where(x => !(x > 10 && x % 10 == 0))
                .Select(x =>
                {
                    var bx = new BigInteger(x);
                    return BigInteger.ModPow(bx, bx, limit);
                })
                .Sum();

            while (sum > limit)
                sum -= limit;

            return sum.ToString();
        }
    }
}
