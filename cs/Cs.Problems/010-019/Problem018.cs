// Author: Aaron Johnson
// Date:   2015-02-12
//
// Solves Euler Problem 18.

using System;
using System.Collections.Generic;
using System.Linq;

namespace euler
{
    /// <title>Maximum path sum I</title>
    /// <summary>
    /// By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
    /// 
    ///     3
    ///    7 4
    ///   2 4 6
    ///  8 5 9 3
    /// 
    /// That is, 3 + 7 + 4 + 9 = 23.
    /// 
    /// Find the maximum total from top to bottom of the triangle below:
    /// 
    ///                75 
    ///               95 64 
    ///              17 47 82 
    ///             18 35 87 10 
    ///            20 04 82 47 65 
    ///           19 01 23 75 03 34 
    ///          88 02 77 73 07 63 67 
    ///         99 65 04 28 06 16 70 92 
    ///        41 41 26 56 83 40 80 70 33 
    ///       41 48 72 33 47 32 37 16 94 29 
    ///      53 71 44 65 25 43 91 52 97 51 14
    ///     70 11 33 28 77 73 17 78 39 68 17 57
    ///    91 71 52 38 17 14 91 43 58 50 27 29 48
    ///   63 66 04 68 89 53 67 30 73 16 69 87 40 31
    ///  04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
    /// 
    /// </summary>
    /// <answer></answer>
    public class Problem018 : IProblem
    {
        private static List<int> PullNumbers(string s)
        {
            return s.Split(' ').Select(n => Convert.ToInt32(n)).ToList();
        }
        public int Solve(string s)
        {
            var rows = s.Split('\n');
            var previousRow = PullNumbers(rows[0]).ToList();

            // each index i in the next row corresponds to i-1 and i in the previous rows (after checking that exist)
            // the maximum of those is added to the current row, which becomes the previous row
            // the current row represents the maximum path ending in that point at that row
            foreach (var currentRow in rows.Skip(1).Select(PullNumbers))
            {
                for (var i = 0; i < currentRow.Count; i++)
                {
                    if (i >= previousRow.Count)
                    {
                        currentRow[i] += previousRow[i - 1];
                    }
                    else if (i - 1 < 0)
                    {
                        currentRow[i] += previousRow[i];
                    }
                    else
                    {
                        int curValue = currentRow[i], fromLeft = previousRow[i - 1], fromRight = previousRow[i];
                        currentRow[i] = (int)Math.Max(curValue + fromLeft, curValue + fromRight);
                    }
                }
                previousRow = currentRow;
            }
            return previousRow.Max();
        }
        public string Solve()
        {
            var start = "" +
                        "75\n" +
                        "95 64\n" +
                        "17 47 82\n" +
                        "18 35 87 10\n" +
                        "20 04 82 47 65\n" +
                        "19 01 23 75 03 34\n" +
                        "88 02 77 73 07 63 67\n" +
                        "99 65 04 28 06 16 70 92\n" +
                        "41 41 26 56 83 40 80 70 33\n" +
                        "41 48 72 33 47 32 37 16 94 29\n" +
                        "53 71 44 65 25 43 91 52 97 51 14\n" +
                        "70 11 33 28 77 73 17 78 39 68 17 57\n" +
                        "91 71 52 38 17 14 91 43 58 50 27 29 48\n" +
                        "63 66 04 68 89 53 67 30 73 16 69 87 40 31\n" +
                        "04 62 98 27 23 09 70 98 73 93 38 53 60 04 23";
            return Solve(start).ToString();
        }
    }
}
