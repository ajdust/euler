using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Circular primes</title>
    /// <summary>
    /// The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
    /// 
    /// There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
    /// 
    /// How many circular primes are there below one million?
    /// </summary>
    public class Problem035 : IProblem
    {
        private static readonly IReadOnlyDictionary<char,int> CharInts = new Dictionary<char, int>
        {
            ['0'] = 0, ['1'] = 1, ['2'] = 2, ['3'] = 3, ['4'] = 4,
            ['5'] = 5, ['6'] = 6, ['7'] = 7, ['8'] = 8, ['9'] = 9
        };

        private int ToInt(IList<char> a)
        {
            var sum = 0;
            var radix = 1;
            for (var i = a.Count - 1; i >= 0; i--)
            {
                sum += CharInts[a[i]] * radix;
                radix *= 10;
            }
            return sum;
        }

        private void RotateLeft(char[] a)
        {
            char temp = a[0];
            int i = 0;
            for (; i < a.Length - 1; i++)
            {
                a[i] = a[i + 1];
            }
            a[i] = temp;
        }

        private List<int> Rotations(int n)
        {
            var chars = n.ToString().ToCharArray();
            var rotations = new List<int>(chars.Length);
            rotations.Add(n);

            if (n < 10)
            {
                return rotations;
            }

            var i = chars.Length - 1;
            do
            {
                RotateLeft(chars);
                rotations.Add(ToInt(chars));
            }
            while (--i > 0);

            return rotations;
        }

        private IEnumerable<int> BruteSolve()
        {
            var primesCalculator = new Primes(1000000);
            var primes = new HashSet<int>(primesCalculator.PrimeList());

            var circularPrimes = new HashSet<int>();

            foreach (var prime in primes)
            {
                if (circularPrimes.Contains(prime))
                    continue;

                var rotations = Rotations(prime);
                if (rotations.All(x => primes.Contains(x)))
                {
                    rotations.ForEach(rotation => circularPrimes.Add(rotation));
                }
            }

            return circularPrimes;
        }

        public string Solve()
        {
            var circulars = BruteSolve();
            return circulars.Count().ToString();
        }
    }
}
