using System;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Triangle Perimeter Sum</title>
    /// <summary>
    /// If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
    /// 
    /// {20,48,52}, {24,45,51}, {30,40,50}
    /// 
    /// For which value of p <= 1000, is the number of solutions maximised?
    /// </summary>
    public class Problem039 : IProblem
    {
        private int BruteSolve()
        {
            var maxPerimeter = Sequences.PythagoreanTriplets()
                .TakeWhile(x => x.Item1 + x.Item2 + x.Item3 < 1000)
                .ToLookup(x => x.Item1 + x.Item2 + x.Item3)
                .Aggregate(Tuple.Create(0,0), (maxes, perimeters) =>
                    perimeters.Count() > maxes.Item1 ? Tuple.Create(perimeters.Count(), perimeters.Key) : maxes);

            return maxPerimeter.Item2;
        }

        public string Solve()
        {
            return BruteSolve().ToString();
        }
    }
}
