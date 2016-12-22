using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Cs.Problems
{
    /// <title>Square root convergents</title>
    /// <summary>
    /// It is possible to show that the square root of two can be expressed as an infinite
    /// continued fraction.
    ///
    /// v2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
    ///
    /// By expanding this for the first four iterations, we get:
    ///
    /// 1 + 1/2 = 3/2 = 1.5
    /// 1 + 1/(2 + 1/2) = 7/5 = 1.4
    /// 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
    /// 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
    ///
    /// The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion,
    /// 1393/985, is the first example where the number of digits in the numerator exceeds the
    /// number of digits in the denominator.
    ///
    /// In the first one-thousand expansions, how many fractions contain a numerator with more
    /// digits than denominator?
    /// </summary>
    public class Problem057 : IProblem
    {
        public class Rational
        {
            public BigInteger Numerator;
            public BigInteger Denominator;

            public Rational(BigInteger num, BigInteger den)
            {
                Numerator = num;
                Denominator = den;
            }

            public Rational Clone() => new Rational(Numerator, Denominator);
            public void SetToReciprocal()
            {
                var temp = Denominator;
                Denominator = Numerator;
                Numerator = temp;
            }
        }

        private IEnumerable<Rational> Expansions()
        {
            // first expansion
            yield return new Rational(3, 2);

            var r = new Rational(5, 2);
            var count = 0;

            do
            {
                // equivalent to adding 1
                var r_ = r.Clone();
                r_.SetToReciprocal();
                r_.Numerator += r_.Denominator;
                yield return r_;

                count++;

                // equivalent to 2 + 1/r
                r.SetToReciprocal();
                r.Numerator += r.Denominator * 2;
            } while (true);
        }

        public string Solve()
        {
            return Expansions()
                .Take(999).Reverse()
                .Count(x => x.Numerator.ToString().Length > x.Denominator.ToString().Length)
                .ToString();
        }
    }
}
