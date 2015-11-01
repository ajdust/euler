using System.Linq;

namespace euler
{
    /// <title>Multiples of 3 and 5</title>
    /// <summary>
    /// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6
    /// and 9. The sum of these multiples is 23. Find the sum of all the multiples of 3 or 5 
    /// below 1000.
    /// </summary>
    /// <remarks>
    /// This can be solved more efficiently with a formula.
    /// 
    /// The sum of the multiples of numbers can be given a formula. The set of multiples
    /// of 3 and 5 intersect at the multiples of 15. So the solution is the sum of the 
    /// multiples of 3 added to the sum of the multiples of 5 minus the sum of the
    /// multiples of 15.
    /// 
    /// The sum of natural numbers from 1 to n = n(n+1)/2
    /// The sum of the first n multiples of 3  = 3n(n+1)/2
    /// The sum of the first n multiples of 5  = 5n(n+1)/2
    /// The sum of the first n multiples of 15 = 15n(n+1)/2
    /// 
    /// Simple integer division can find the size of n for a number at or under k and multiple m:
    ///     n = k / m
    /// 
    /// Hence, given a limit of multiples under k, the sum S for this problem can be computed:
    ///     S = 3(k/3)((k/3)+1)/2 + 5(k/5)((k/5)+1)/2 - 15(k/15)((k/15)+1)/2
    /// 
    /// With k = 999 this equates to 233168.
    /// </remarks>
    /// <answer>233168</answer>
    public class Problem001 : IProblem
    {
        static int FormulaSolve(int limit)
        {
            var k = limit;
            int n3 = k / 3, n5 = k / 5, n15 = k / 15;
            return 3 * n3 * (n3 + 1) / 2
                + 5 * n5 * (n5 + 1) / 2
                - 15 * n15 * (n15 + 1) / 2;
        }
        static int BruteSolve(int limit)
        {
            return Enumerable.Range(1, limit).Where(n => n % 3 == 0 || n % 5 == 0).Sum();
        }
        public string Solve()
        {
            return FormulaSolve(999).ToString();
        }
    }
}
