using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Pandigital Concatenated</title>
    /// <summary>
    /// Take the number 192 and multiply it by each of 1, 2, and 3:
    /// 
    ///     192 × 1 = 192
    ///     192 × 2 = 384
    ///     192 × 3 = 576
    /// 
    /// By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
    /// 
    /// The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
    /// 
    /// What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
    /// </summary>
    public class Problem038 : IProblem
    {
        public IEnumerable<int> PandigitalNumbers()
        {
            return Combinatorics
                .PermuteBackward(new[] { '9', '8', '7', '6', '5', '4', '3', '2', '1' })
                .Select(x => Convert.ToInt32(new string(x)));
        }

        private int HasConcatenatedPandigital(int n)
        {
            var digits = new List<char>(12);
            var uniqueDigits = new HashSet<char>();

            for (int i = 1; i <= 6; i++)
            {
                var newDigits = (n * i).ToString();

                digits.AddRange(newDigits);
                if (digits.Count > 9)
                    return -1;

                foreach (var c in newDigits)
                {
                    if (c == '0')
                        return -1;
                    if (uniqueDigits.Contains(c))
                        return -1;
                    uniqueDigits.Add(c);
                }

                if (uniqueDigits.Count == 9)
                    break;
            }

            return Convert.ToInt32(new string(digits.ToArray()));
        }

        private int BruteSolve()
        {
            // 9999 is an upper bound - 4 digits (x1) + 5 digits (x2) is 9 digits
            return Enumerable.Range(1, 9999)
                // assuming they didn't give the answer away in the description,
                // the first digit must be 9 for the pandigital to start with 9 and be larger than 918273645
                .Where(x => x.ToString()[0] == '9')
                .Select(x => HasConcatenatedPandigital(x))
                .Max();
        }

        public string Solve()
        {
            return BruteSolve().ToString();
        }
    }
}
