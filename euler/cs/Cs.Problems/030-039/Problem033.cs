using System;
using System.Collections.Generic;
using System.Linq;

namespace Cs.Problems
{
    /// <title>Unorthodox Fractions</title>
    /// <summary>
    /// The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to
    /// simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling
    /// the 9s.
    /// 
    /// We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
    /// 
    /// There are exactly four non-trivial examples of this type of fraction, less than one in value, and
    /// containing two digits in the numerator and denominator.
    /// 
    /// If the product of these four fractions is given in its lowest common terms, find the value of the
    /// denominator.
    /// </summary>
    public class Problem033 : IProblem
    {
        public static long GetGreatestCommonFactor(long a, long b)
        {
            var aPrimeFactors = Sequences.GetPrimeFactors(a).GroupBy(x => x).ToDictionary(x => x.Key, x => x.Count());
            var bPrimeFactors = Sequences.GetPrimeFactors(b).GroupBy(x => x).ToDictionary(x => x.Key, x => x.Count());

            // collect the factors between the two, similar to Problem005,
            // except this time pool the shared minimums
            var minCount = new Dictionary<long, int>();
            foreach (var factor in aPrimeFactors.Concat(bPrimeFactors))
            {
                if (aPrimeFactors.ContainsKey(factor.Key) && bPrimeFactors.ContainsKey(factor.Key))
                    minCount[factor.Key] = minCount.ContainsKey(factor.Key)
                      ? Math.Min(minCount[factor.Key], factor.Value)
                      : factor.Value;
            }

            return minCount.Aggregate<KeyValuePair<long, int>, long>(
                1, (current, factor) => current * (long)Math.Pow(factor.Key, factor.Value));
        } 

        private struct Fraction : IEquatable<Fraction>
        {
            public Fraction(long numerator, long denominator)
            {
                Numerator = numerator;
                Denominator = denominator;
            }

            public readonly long Numerator;
            public readonly long Denominator;

            public Fraction Reduce()
            {
                var greatestCommonFactor = GetGreatestCommonFactor(Numerator, Denominator);
                return new Fraction(Numerator/greatestCommonFactor, Denominator/greatestCommonFactor);
            }

            public static Fraction operator *(Fraction left, Fraction right)
            {
                return new Fraction(left.Numerator*right.Numerator, left.Denominator*right.Denominator);
            }

            public bool Equals(Fraction other)
            {
                return Numerator == other.Numerator
                       && Denominator == other.Denominator;
            }

            public static bool operator ==(Fraction left, Fraction right)
            {
                return left.Equals(right);
            }

            public static bool operator !=(Fraction left, Fraction right)
            {
                return !left.Equals(right);
            }

            public override bool Equals(object obj)
            {
                switch (obj)
                {
                    case null: return false;
                    case Fraction f: return Equals(f);
                    default: return false;
                }
            }

            public override int GetHashCode() => 193 ^ this.Numerator.GetHashCode() ^ this.Denominator.GetHashCode();
        }

        private long ToLong(long tens, long ones)
        {
            return tens * 10 + ones;
        }

        private IEnumerable<Fraction> BruteSolve()
        {
            // - there are only a set number of fractions possible in these 'cancellations'
            // - if any digit is zero, or first digits or last digits are shared, then it is trivial and not counted
            for (int i = 1; i <= 9; i++)
            {
                for (int j = i+1; j <= 9; j++)
                {
                    // consider i/j:
                    // this must be part of either ki/jk or ik/kj

                    var ratio = new Fraction(i, j).Reduce();

                    // ki/jk
                    for (int k = 1; k < j; k++)
                    {
                        var num = ToLong(k, i);
                        var den = ToLong(j, k);
                        var fraction = new Fraction(num, den);
                        if (fraction.Reduce() == ratio)
                        {
                            yield return fraction;
                        }
                    }

                    // ik/kj
                    for (int k = i+1; k <= 9; k++)
                    {
                        var num = ToLong(i, k);
                        var den = ToLong(k, j);
                        var fraction = new Fraction(num, den);
                        if (fraction.Reduce() == ratio)
                        {
                            yield return fraction;
                        }
                    }
                }
            }
        }

        public string Solve()
        {
            var results = BruteSolve().ToList();
            return results.Aggregate((x, y) => x*y).Denominator.ToString();
        }
    }
}
