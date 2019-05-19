// Author: Aaron Johnson
// Date:   2015-05-25
//
// Sovles Euler problem 31.

using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Coin sums</title>
    /// <summary>
    /// 
    /// In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
    /// 
    ///     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
    /// 
    /// It is possible to make £2 in the following way:
    /// 
    ///     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
    /// 
    /// How many different ways can £2 be made using any number of coins?
    /// </summary>
    public class Problem031 : IProblem
    {
        // N, the number of ways to make amount A using using coins C = { c_1, c_2, c_3, c_4,  ... c_n }
        // N(A, C) = N(A, C - {c_1}) + N(A - c_1, C)
        // in other words, sum the groups adding to A that do not use c_1, and the groups adding to A that use at least one c_1

        // The naive inefficient purely recursive solution
        private int NumCombRecursive(int sum, IEnumerable<int> amounts)
        {
            if (sum < 0 || !amounts.Any())
                return 0;

            if (sum == 0)
                return 1;

            return NumCombRecursive(sum, amounts.Skip(1)) + NumCombRecursive(sum - amounts.First(), amounts);
        }

        // The less naive far more efficient memoized recursive solution
        private Dictionary<string,int> _memo = new Dictionary<string,int>();

        private int NumCombRecursiveMemoized(int sum, IEnumerable<int> amounts)
        {
            var key = sum.ToString() + " " + string.Join(" ", amounts);
            if (_memo.ContainsKey(key))
                return _memo[key];

            if (sum < 0 || !amounts.Any())
                return 0;

            if (sum == 0)
                return 1;

            var answer = NumCombRecursiveMemoized(sum, amounts.Skip(1)) + NumCombRecursiveMemoized(sum - amounts.First(), amounts);
            _memo[key] = answer;
            return answer;
        }

        public string Solve()
        {
            return NumCombRecursiveMemoized(200, new SortedSet<int> {1, 2, 5, 10, 20, 50, 100, 200}).ToString();
        }
    }
}