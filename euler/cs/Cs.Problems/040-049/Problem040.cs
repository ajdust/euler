using System;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Champernowne Number</title>
    /// <summary>
    /// An irrational decimal fraction is created by concatenating the positive integers:
    /// 0.12345678910 1 112131415161718192021...
    /// It can be seen that the 12th digit of the fractional part is 1.
    /// If d_n represents the nth digit of the fractional part, find the value of the following expression.
    /// d_1 × d_10 × d_100 × d_1000 × d_10000 × d_100000 × d_1000000
    /// </summary>
    public class Problem040 : IProblem
    {
        public char this[int i]
        {
            get
            {
                var index = 0;
                var num = -1;
                var numstr = "";
                while (index < i)
                {
                    num += 1;
                    numstr = num.ToString();
                    index += numstr.Length;
                }

                return numstr.Reverse().ToArray()[(i - index) * -1];
            }
        }

        public string Solve()
        {
            var digits = new[] { 1, 10, 100, 1000, 10000, 100000, 1000000 }
                .Select(i => (int)Char.GetNumericValue(this[i + 1]));
            return digits.Product().ToString();
        }
    }
}
