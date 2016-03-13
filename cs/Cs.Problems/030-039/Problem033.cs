using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

namespace Cs.Problems
{
    public static class CharExtensions
    {
        private static IReadOnlyDictionary<char, long> Digit = new ReadOnlyDictionary<char, long>(new Dictionary<char,long>
        {
            { '0', 1 }, { '1', 1 }, { '2', 2 }, { '3', 3 }, { '4', 4 },
            { '5', 5 }, { '6', 6 }, { '7', 7 }, { '8', 8 }, { '9', 9 }
        });

        public static long Tolong(this char c)
        {
            return Digit[c];
        }
    }

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
        public static IEnumerable<long> CommonFactors(long a, long b)
        {
            var commonFactors = new HashSet<long>(Sequences.GetPrimeFactors(a));
            commonFactors.IntersectWith(Sequences.GetPrimeFactors(b));
            return commonFactors;
        } 

        private struct Fraction
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
                var biggestCommonFactor = CommonFactors(Numerator, Denominator).Max();
                return new Fraction(Numerator/biggestCommonFactor, Denominator/biggestCommonFactor);
            }

            public static Fraction operator *(Fraction left, Fraction right)
            {
                return new Fraction(left.Numerator*right.Numerator, left.Denominator*right.Denominator);
            }
        }

        private long Tolong(char tens, char ones)
        {
            return tens.Tolong() * 10 + ones.Tolong();
        }

        private IEnumerable<Fraction> BruteSolve()
        {
            // - there are only a set number of fractions possible in these 'cancellations'
            // - if any digit is zero, or first digits or last digits are shared, then it is trivial and not counted
            for (char i = '1'; i <= '9'; i++)
            {
                for (char j = (char)(i+1); j <= '9'; j++)
                {
                    // consider i/j:
                    // this must be part of either ki/jk or ik/kj

                    var ratio = Convert.ToDouble(i.Tolong()) / Convert.ToDouble(j.Tolong());

                    // ki/jk
                    for (char k = '1'; k < j; k++)
                    {
                        var num = Tolong(k, i);
                        var den = Tolong(j, k);
                        if (Convert.ToDouble(num)/Convert.ToDouble(den) == ratio)
                        {
                            yield return new Fraction(num, den);
                        }
                    }

                    // ik/kj
                    for (char k = (char)(i+1); k <= '9'; k++)
                    {
                        var num = Tolong(i, k);
                        var den = Tolong(k, j);
                        if (Convert.ToDouble(num) / Convert.ToDouble(den) == ratio)
                        {
                            yield return new Fraction(num, den);
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
