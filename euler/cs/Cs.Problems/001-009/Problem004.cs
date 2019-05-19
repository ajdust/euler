using System.Linq;

namespace Cs.Problems
{
    /// <title>Largest palindrome product</title>
    /// <summary>
    /// A palindromic number reads the same both ways. The largest palindrome
    /// made from the product of two 2-digit numbers is 9009 = 91 × 99.
    /// Find the largest palindrome made from the product of two 3-digit numbers.
    /// </summary>
    /// <answer>906609</answer>
    public class Problem004 : IProblem
    {
        static bool IsPalindrome(string s)
        {
            for (int a = 0, b = s.Length - 1; a < b; a++, b--)
            {
                if (s[a] != s[b])
                {
                    return false;
                }
            }
            return true;
        }
        static int BruteSolve()
        {
            return (from i in Enumerable.Range(100, 899)
                    from j in Enumerable.Range(100, 899)
                    where IsPalindrome((i * j).ToString())
                    select i * j).Max();
        }

        public string Solve()
        {
            return BruteSolve().ToString();
        }
    }
}
