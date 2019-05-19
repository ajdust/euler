using System;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Same Digit Multiples</title>
    /// <summary>
    /// It can be seen that the number, 125874, and its double, 251748, contain exactly the same
    /// digits, but in a different order.
    /// 
    /// Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the
    /// same digits.
    /// </summary>
    public class Problem052 : IProblem
    {
        private bool SameDigits(params int[] numbers)
        {
            var digitStrings = numbers.Select(x =>
            {
                var a = x.ToString().ToCharArray();
                Array.Sort(a);
                return new string(a);
            });

            var first = digitStrings.First();
            return digitStrings.Skip(1).All(x => x == first);
        }
        public string Solve()
        {
            // x*6 will give more digits than the original number if the number
            // is greater than 1/6 of 10^n where n is the number of digits
            var result = Enumerable.Range(2, 8)
                .SelectMany(x => Enumerable.Range(10 ^ x, (int)Math.Ceiling((1.0 / 6.0) * Math.Pow(10.0, x))))
                .First(x => SameDigits(x, x * 2, x * 3, x * 4, x * 5, x * 6));

            return result.ToString();
        }
    }
}
