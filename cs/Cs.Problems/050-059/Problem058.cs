using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Spiral primes</title>
    /// <summary>
    /// Starting with 1 and spiralling anticlockwise in the following way, a square spiral with
    /// side length 7 is formed.
    ///
    /// 37 36 35 34 33 32 31
    /// 38 17 16 15 14 13 30
    /// 39 18  5  4  3 12 29
    /// 40 19  6  1  2 11 28
    /// 41 20  7  8  9 10 27
    /// 42 21 22 23 24 25 26
    /// 43 44 45 46 47 48 49
    ///
    /// It is interesting to note that the odd squares lie along the bottom right diagonal,
    /// but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are
    /// prime; that is, a ratio of 8/13 ˜ 62%.
    ///
    /// If one complete new layer is wrapped around the spiral above, a square spiral with side
    /// length 9 will be formed. If this process is continued, what is the side length of the
    /// square spiral for which the ratio of primes along both diagonals first falls below 10%?
    /// </summary>
    public class Problem058 : IProblem
    {
        private struct SideLengthAndRatio
        {
            public SideLengthAndRatio(long length, int numberPrime, int numberDiagonals)
            {
                Length = length;
                NumberPrime = numberPrime;
                NumberDiagonals = numberDiagonals;
            }

            public double Ratio => NumberPrime / (double)NumberDiagonals;
            public long Length;
            public int NumberPrime;
            public int NumberDiagonals;
        }

        private IEnumerable<SideLengthAndRatio> SideLengthAndRatios()
        {
            var primeCalculate = new Primes(1000000000);
            primeCalculate.CalculatePrimes();
            var primes = primeCalculate.PrimeList().GetEnumerator();
            var numberDiagonals = 1;
            var numberPrimes = 0;

            for (long length = 3;; length += 2)
            {
                var corner = length * length;
                var step = length - 1;

                // create small prime checking set
                var primeSet = new HashSet<long>();
                while (primes.MoveNext())
                {
                    var next = primes.Current;
                    if (next > corner)
                        break;
                    primeSet.Add(next);
                }

                // exclude corner as it is clearly not prime
                numberPrimes += new[] { corner - step, corner - step*2, corner - step*3 }
                    .Count(x => primeSet.Contains(x));
                numberDiagonals += 4;
                yield return new SideLengthAndRatio(length, numberPrimes, numberDiagonals);
            }
        }

        public string Solve()
        {
            return SideLengthAndRatios().First(x => x.Ratio < 0.10).Length.ToString();
        }
    }
}
