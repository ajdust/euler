// Author: Aaron Johnson
// Date:   2015-02-12
// 
// Solves Euler Problem 17.

using System;
using System.Collections.Generic;
using System.Linq;

namespace euler
{

    /// <title>Number letter counts</title>
    /// <summary>
    /// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
    /// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
    /// 
    /// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen)
    /// contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
    /// 
    /// </summary>
    /// <remarks>While the question only asks for numbers 1 to 1000, I extended it a bit.</remarks>
    /// <answer>21124</answer>
    public class Problem017 : IProblem
    {
        private static readonly Dictionary<int, int> NumberToEnglishLetterCount = new Dictionary<int, int>()
        {
            {0, "zero".Length},
            {1, "one".Length},
            {2, "two".Length},
            {3, "three".Length},
            {4, "four".Length},
            {5, "five".Length},
            {6, "six".Length},
            {7, "seven".Length},
            {8, "eight".Length},
            {9, "nine".Length},
            {10, "ten".Length},
            {11, "eleven".Length},
            {12, "twelve".Length},
            {13, "thirteen".Length},
            {14, "fourteen".Length},
            {15, "fifteen".Length},
            {16, "sixteen".Length},
            {17, "seventeen".Length},
            {18, "eighteen".Length},
            {19, "nineteen".Length},
            {20, "twenty".Length},
            {30, "thirty".Length},
            {40, "forty".Length},
            {50, "fifty".Length},
            {60, "sixty".Length},
            {70, "seventy".Length},
            {80, "eighty".Length},
            {90, "ninety".Length},
            {100, "hundred".Length},
        };

        private static readonly Dictionary<int, int> ThreeDigitSectionNames = new Dictionary<int, int>()
        {
            {0, "".Length},
            {1, "thousand".Length},
            {2, "million".Length},
            {3, "billion".Length}
        };

        /// <summary>
        /// Counts the number of letters for English-spelled numbers between 0 and 999.
        /// </summary>
        /// <returns>The number of letters in the English spelling.</returns>
        private static int CountEnglishNumberGroup(int n)
        {
            if (n > 999 || n < 0)
            {
                throw new ArgumentOutOfRangeException("n");
            }

            if (n == 0)
            {
                return 0;
            }

            var count = 0;
            var hundreds = 0;
            var tens = 0;
            var ones = 0;

            // hundreds place
            if (n >= 100)
            {
                hundreds = n / 100;
                n -= hundreds * 100;
            }

            // tens place
            if (n >= 20)
            {
                tens = n / 10;
                n -= tens * 10;
            }

            // below twenty and ones place
            if (n >= 1)
            {
                ones = n;
            }

            // the counting
            if (hundreds > 0)
            {
                count += NumberToEnglishLetterCount[hundreds];
                count += NumberToEnglishLetterCount[100];
            }

            if (tens > 0)
            {
                count += NumberToEnglishLetterCount[tens * 10];
            }

            if (ones > 0)
            {
                count += NumberToEnglishLetterCount[ones];
            }

            if (hundreds > 0 && (tens > 0 || ones > 0))
            {
                count += 3; // hundred "and" tens
            }

            return count;
        }

        public static int GetEnglishNumberLetterCount(int n)
        {
            // pad the number to be divisible by three for grouping convenience
            var radix = n.ToString();
            var addPad = Primes.Mod(-radix.Length, 3);
            radix = radix.PadLeft(radix.Length + addPad);

            var count = 0;
            foreach (var t in radix.SplitUpIntoGroups(3).Enumerate())
            {
                var i = t.Item1;
                var grp = t.Item2;
                var numbers = grp.Aggregate("", (prev, next) => prev + next);
                count += CountEnglishNumberGroup(Convert.ToInt32(numbers));
                count += ThreeDigitSectionNames[(int)i];
            }
            return count;
        }
        public string Solve()
        {
            return Enumerable.Range(1, 1000).Select(GetEnglishNumberLetterCount).Sum().ToString();
        }
    }
}
