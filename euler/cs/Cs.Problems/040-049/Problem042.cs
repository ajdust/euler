using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Alpha Value and Triangle Numbers</title>
    /// <summary>
    /// The nth term of the sequence of triangle numbers is given by, t_n = n(n+1)/2; so the first ten triangle numbers are:
    /// 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
    ///
    /// By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
    /// For example, the word value for SKY is 19 + 11 + 25 = 55 = t_10. If the word value is a triangle number then we shall call the word a triangle word.
    ///
    /// Using words.txt, a 16K text file containing nearly two-thousand common English words, how many are triangle words?
    /// 
    /// </summary>
    public class Problem042 : IProblem
    {
        private static IEnumerable<string> GetWords()
        {
            var contents = Resources.Resources.Get("Problem042_words.txt");
            return contents.Split(',').Select(word => word.Trim('"'));
        }

        private static long GetWordValue(string word)
        {
            return word.ToUpper().Sum(x => x - 64);
        }

        public string Solve()
        {
            var words = GetWords();
            var wordValues = words.Select(GetWordValue).ToArray();
            var maxValue = wordValues.Max();
            var triangleValues = new HashSet<long>(Sequences.TriangleNumbers().TakeWhile(x => x < maxValue));
            return wordValues.Count(x => triangleValues.Contains(x)).ToString();
        }
    }
}
