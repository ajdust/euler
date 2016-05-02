using System;
using System.Linq;

namespace Cs.Problems
{
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
