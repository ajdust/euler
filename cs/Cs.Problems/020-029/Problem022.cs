// Author: Aaron Johnson
// Date:   2015-02-21
// 
// Solves Euler Problem 22.

using System.Collections.Generic;
using System.IO;
using System.Linq;
using Cs.Problems.Resources;

namespace euler
{
    /// <title>Names scores</title>
    /// <summary>
    /// Using names.txt, a 46K text file containing over five-thousand first names, begin by sorting
    /// it into alphabetical order. Then working out the alphabetical value for each name, multiply 
    /// this value by its alphabetical position in the list to obtain a name score. For example, when
    /// the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, 
    /// is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
    /// 
    /// What is the total of all the name scores in the file?
    /// </summary>
    /// <remarks>This problem uses Problem 17's enumerate.</remarks>
    /// <answer>871198282</answer>
    public class Problem022 : IProblem
    {
        private static int WordToValue(string word)
        {
            return word.ToCharArray().Select(c => (int) c).Sum() - 64 * word.Length;
        }
        private static IEnumerable<string> ParseNamesList(string filepath)
        {
            var contents = Resources.Get("Problem022_names.txt");
            return contents.Split('\n').SelectMany(line => line.Split(',')).Select(word => word.Trim('"'));
        } 
        
        public string Solve()
        {
            var names = ParseNamesList("ProblemFiles/Problem022_names.txt").ToList();
            names.Sort();

            var answer = names.Enumerate().Select(t => (t.Item1 + 1)*WordToValue(t.Item2)).Sum();
            return answer.ToString();
        }
    }
}
