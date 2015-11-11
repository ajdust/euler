using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    public static class NumberExtensions
    {
        public static BigInteger Factorial(this int n)
        {
            return n <= 1 ? 1 : Enumerable.Range(1, n).Aggregate(new BigInteger(1), (acc, x) => acc * x);
        }
    }
}
