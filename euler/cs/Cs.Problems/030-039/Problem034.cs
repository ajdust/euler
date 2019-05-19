using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Sum Factorial Digits</title>
    /// <summary>
    /// 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
    /// 
    /// Find the sum of all numbers which are equal to the sum of the factorial of their digits.
    /// 
    /// Note: as 1! = 1 and 2! = 2 are not sums they are not included.
    /// </summary>
    public class Problem034 : IProblem
    {
        /// Notice that the number 9,999,999 applied this way yields 2,540,160.
        /// No matter how you change the numbers, 9,999,999 is going to be bigger than
        /// the sum of its digit factorials.
        /// 
        /// Thus, the limit is 10,000,000.
        /// Test each number up to this.
        public IEnumerable<int> BruteSolve()
        {
            var factorials = new[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }
                .ToDictionary(i => i.ToString().Single(), i => Enumerable.Range(1, i).Aggregate(1, (x,y) => x*y));

            var matches = new List<int>();
            for (int i = 10; i < 10000000; i++)
            {
                if (i.ToString().Sum(x => factorials[x]) == i)
                    matches.Add(i);
            }

            return matches;
        }

        public string Solve()
        {
            var curious = BruteSolve();
            return curious.Sum().ToString();
        }
    }
}
