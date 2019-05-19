using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Palindromic Binary Decimal</title>
    /// <summary>
    /// The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.
    /// Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
    /// (Please note that the palindromic number, in either base, may not include leading zeros.)
    /// </summary>
    public class Problem036 : IProblem
    {
        private bool IsPalindrome(char[] characters)
        {
            var s = characters;
            for (int i = 0, j = s.Length - 1; i < j; i++, j--)
            {
                if (s[i] != s[j])
                    return false;                
            }

            return true;
        }

        private bool IsPalindrome(int n, int toBase)
        {
            var s = Convert.ToString(n, toBase).ToCharArray();
            return IsPalindrome(s);
        }

        private int[] Reflect(int n)
        {
            var s = n.ToString();
            var r = new string(s.Reverse().ToArray());
            var evenReflection = Convert.ToInt32(s + r);
            var oddReflection = Convert.ToInt32(s + r.Substring(1));
            return new[] { evenReflection, oddReflection };
        }

        private IEnumerable<int> PalindromesUnderOneMillion()
        {
            // to compute all base 10 palindromes under 1000000,
            // simply reflect numbers up to 999 evenly and oddly
            return Enumerable.Range(1, 999).SelectMany(Reflect);
        }

        public string Solve()
        {
            var palindromesBothDecimalAndBinary = PalindromesUnderOneMillion()
                .Where(x => IsPalindrome(x, 2));
            return palindromesBothDecimalAndBinary.Sum().ToString();
        }
    }
}
