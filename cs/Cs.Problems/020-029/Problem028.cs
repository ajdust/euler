// Author: Aaron Johnson
// Date:   2015-03-07
//
// Solves Euler problem 28.

namespace Cs.Problems
{
    /// <title>Number spiral diagonals</title>
    /// <summary>
    /// Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
    /// 21 22 23 24 25
    /// 20  7  8  9 10
    /// 19  6  1  2 11
    /// 18  5  4  3 12
    /// 17 16 15 14 13
    ///
    /// It can be verified that the sum of the numbers on the diagonals is 101.
    /// What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
    /// </summary>
    /// <remarks>
    ///	43 44 45 46 47 48 49
    ///	42 21 22 23 24 25 26
    ///	41 20  7  8  9 10 27
    ///	40 19  6  1  2 11 28
    ///	39 18  5  4  3 12 29
    ///	38 17 16 15 14 13 30
    ///	37 36 35 34 33 32 31
    ///
    ///	Just from looking at this, it is clear that the numbers of the right-leaning
    ///	diagonal (/) are squares, or close. 9, 25, 49 are clear, 5, 17, 37 are off by one.
    ///
    ///	The left-leaning diagonal is also related to the squares, in that each number
    ///	can be represented by a square minus its root.
    ///	43 is 49 minus 7 + 1.
    ///	21 is 25 minus 5 + 1.
    ///	On the other side, 31 is 36 minus 6 + 1.
    ///
    ///	Hence, the solution formula contains the sum of squares twice,
    ///	and the sum of numbers (the sum of the roots of the squares) once.
    ///	One sum of squares is for the right (/) diagonal, and the other in combination with the
    ///	sum of roots is for the left (\) diagonal.
    ///
    ///	The only sticky point is the off-by-one error. Let's look at this in more detail.
    ///
    ///	N	n	derived		off-by-one
    ///	1					(+1)
    ///	3	2  	2^2 - 2  	(+1)*
    ///	5	4  	2^2      	(+1)
    ///	7	6  	3^2 - 3  	(+1)*
    ///	9	   	3^2
    ///	13	12 	4^2 - 4		(+1)*
    ///	17	16 	4^2      	(+1)
    ///	21	20 	5^2 - 5  	(+1)*
    ///	25	   	5^2
    ///	...
    ///
    ///	Since 1 is shared by all the sequences, and it should only be added once,
    ///	it can be removed from the summation formulas and added at the end.
    ///
    ///	Notice that the (+1) applies to each square-root difference. This is noted with a *.
    ///	But on the other side it only applies to the even roots. For the * we need +(n-1).
    ///	For the other we need +(n/2 + 1) by including 1.
    /// In total we need +(n+n/2) to get the proper amount.
    ///
    ///	Given the sum of squares is n * (n + 1) * (2 * n + 1) / 6
    ///	Given the sum of numbers is n * (n + 1) / 2
    ///
    ///	The answer is yielded for odd n with
    ///		[SumOfSquares(n) - 1] + [SumOfSquares(n) - 1] - [SumOfNumbers(n) - 1] + n + n/2
    ///	Simplified this is
    ///		2*SumOfSquares(n) - SumOfNumers(n) + n + n/2 - 1
    ///	Further simplification is not really necessary.
    ///
    /// </remarks>
    /// <answer>669171001</answer>
    public class Problem028 : IProblem
    {
        private static long SumOfSquares(long n)
        {
            return n * (n + 1) * (2 * n + 1) / 6;
        }

        private static long SumOfNumbers(long n)
        {
            return n * (n + 1) / 2;
        }

        private long SpiralDiagonalSum(int n)
        {
            return 2 * SumOfSquares(n) - SumOfNumbers(n) + n + n / 2 - 1;
        }

        public string Solve()
        {
            return SpiralDiagonalSum(1001).ToString();
        }
    }
}