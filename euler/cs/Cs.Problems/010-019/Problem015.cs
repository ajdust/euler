// Author: Aaron Johnson
// Date: 2015-02-10
//
// Solves Euler Problem 15.

namespace Cs.Problems
{
    /// <title>Lattice paths</title>
    /// <summary>
    /// Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
    /// How many routes are there through a 20 x 20 grid?
    /// </summary>
    /// <remarks>
    /// This problem can be solved mathematically. Consider each number as the number of paths starting
    /// at that corner in the 2 x 2 grid.
    /// 
    /// 20 10  4  1
    /// 10  6  3  1
    ///  4  3  2  1
    ///  1  1  1  0
    /// 
    /// The number of paths from each corner is equal to the sum of the number of paths going down and right.
    /// This is Pascal's triangle in disguise. Looking further, it is the Central Binomial Coefficient.
    /// 
    /// The formula for the nth number of this sequence is (2n)!/(n!)^2.
    /// 
    /// For a 2 x 2 grid n is 2. 
    /// Hence, for a 20 x 20 grid, n is 20.
    /// 
    /// </remarks>
    /// <answer></answer>
    public class Problem015 : IProblem
    {
        public string Solve()
        {
            // note that (2n!) / (n!)^2  == (2n * (2n-1) * (2n-2) * n) / n!
            var ans = Sequences.LongRange(21, 40).Product() / 20.Factorial();
            return ans.ToString();

            //var nfac = Factorial(20);
            //return (Factorial(20 * 2) / (nfac * nfac)).ToString();
        }
    }
}
