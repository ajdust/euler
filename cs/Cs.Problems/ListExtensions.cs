using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    public static class ListExtensions
    {
        public static long Product(this IEnumerable<int> numbers)
        {
            long p = 1;
            foreach (var number in numbers)
            {
                checked { p *= number; }
            }
            return p;
        }

        public static BigInteger Product(this IEnumerable<long> numbers)
        {
            return numbers.Aggregate(new BigInteger(1), (acc, x) => acc * x);
        }

        public static BigInteger Sum(this IEnumerable<BigInteger> bigints)
        {
            return bigints.Aggregate<BigInteger, BigInteger>(0, (current, n) => current + n);
        }
        
        /// <summary>
        /// Split an enumerable into lists of size n.
        /// </summary>
        /// <param name="objects"></param>
        /// <param name="n"></param>
        /// <returns>List</returns>
        public static IEnumerable<List<T>> SplitUpIntoGroups<T>(this IEnumerable<T> objects, int n)
        {
            var enumerator = objects.GetEnumerator();
            var yieldList = new List<T>();

            while (enumerator.MoveNext())
            {
                yieldList.Add(enumerator.Current);
                if (yieldList.Count != n) continue;
                yield return yieldList;
                yieldList.Clear();
            }

            if (yieldList.Any())
                yield return yieldList;
        }

        public static IEnumerable<Tuple<long, T>> Enumerate<T>(this IEnumerable<T> objects)
        {
            var count = 0;
            foreach (var obj in objects)
            {
                yield return new Tuple<long, T>(count, obj);
                count++;
            }
        }
    }
}
