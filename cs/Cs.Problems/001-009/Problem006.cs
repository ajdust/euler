using System;

namespace Cs.Problems
{
    /// <title>Sum square difference</title>
    /// <summary>
    /// The sum of the squares of the first ten natural numbers is,
    /// 12 + 22 + ... + 102 = 385 
    /// The square of the sum of the first ten natural numbers is,
    /// (1 + 2 + ... + 10)2 = 552 = 3025
    /// Hence the difference between the sum of the squares of the first ten natural numbers
    /// and the square of the sum is 3025 − 385 = 2640.
    /// 
    /// Find the difference between the sum of the squares of the first one hundred natural
    /// numbers and the square of the sum.
    /// </summary>
    /// <remarks>
    /// This problem can be solved with brute force easily, but it is better to use the formulas.
    /// Formula for sum of squares:    1^2 + 2^2 + 3^2 ... + n^2 = n(n+1)(2n+1)/6 
    /// Formula for square of the sum: (1 + 2 + 3 + ... + n)^2   = (n(n+1)/2)^2
    /// </remarks>
    /// <answer>25164150</answer>
    public class Problem006 : IProblem
    {
        static long SumOfSquares(long n)
        {
            return n * (n + 1) * (2 * n + 1) / 6;
        }

        static long SumOfNumbers(long n)
        {
            return n * (n + 1) / 2;
        }
        static long Solve(long n)
        {
            return Math.Abs(SumOfSquares(n) - (long)Math.Pow(SumOfNumbers(n), 2));
        }
        public string Solve()
        {
            return Solve(100).ToString();
        }
    }
}
