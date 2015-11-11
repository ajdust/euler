// Author: Aaron Johnson
// Date:   2015-02-22
//
// Solves Euler Problem 24.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Lexicographic permutations</title>
    /// <summary>
    /// A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
    ///
    /// 012   021   102   120   201   210
    ///
    /// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
    ///
    /// </summary>
    /// <remarks>This problem uses Problem 15's Factorial. Still surprised it's not a method of BigInteger. </remarks>
    /// <answer>2783915460</answer>
    public class Problem024 : IProblem
    {
        public static List<int> GetPermutationNIndexes(int length, BigInteger n)
        {
            var distance = n;
            var pIndexes = Enumerable.Range(0, length).Select(x => -1).ToList();
            var indexPool = Enumerable.Range(0, length).ToList();

            for (var curIndex = 0; curIndex < length; curIndex++)
            {
                var permutationCounter = (length - curIndex - 1).Factorial(); // how many permutations there are between steps at this counter
                var pIndex = (int) (distance / permutationCounter);                   // how many permutation counts we can deal out without going over the limit

                pIndexes[curIndex] = indexPool[pIndex];               // pop from the index pool into the permutation index
                indexPool.RemoveAt(pIndex);
                distance -= pIndex * permutationCounter;              // reduce by the distance we travelled
            }

            return pIndexes;
        }
        public string Solve()
        {
            var indexes = GetPermutationNIndexes(10, 999999);
            return String.Join("", indexes);
        }
    }
}