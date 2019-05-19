using System;

namespace Cs.Problems
{
    /// <title>Special Pythagorean triplet</title>
    /// <summary>
    /// A Pythagorean triplet is a set of three natural numbers, a LT b LT c, for which,
    /// a^2 + b^2 = c^2
    /// 
    /// For example, 32 + 42 = 9 + 16 = 25 = 52.
    /// 
    /// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    /// Find the product a*b*c. 
    /// </summary>
    /// <remarks>Is this the most efficient solution to this problem? It takes almost half a second to compute.</remarks>
    /// <answer>31875000</answer>
    public class Problem009 : IProblem
    {
        static Tuple<int, int, int> SolveAbcAddTo1000()
        {
            foreach (var triplet in Sequences.PythagoreanTriplets())
            {
                var a = triplet.Item1;
                var b = triplet.Item2;
                var c = triplet.Item3;
                if (a + b + c == 1000)
                {
                    return new Tuple<int, int, int>(a, b, c);
                }
                if (a + b + c > 2000)
                {
                    break;
                }
            }
            return null;
        }

        public string Solve()
        {
            var triplet = SolveAbcAddTo1000();
            return (triplet.Item1 * triplet.Item2 * triplet.Item3).ToString();
        }
    }
}
