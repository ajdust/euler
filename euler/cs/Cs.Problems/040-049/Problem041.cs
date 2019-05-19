using System;

namespace Cs.Problems
{
    /// <title>Pandigital Primes</title>
    /// <summary>
    /// We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.For example, 2143 is a 4-digit pandigital and is also prime.
    /// What is the largest n-digit pandigital prime that exists?
    /// </summary>
    public class Problem041 : IProblem
    {
        private bool IsPandigital(int prime)
        {
            var digits = prime.ToString().ToCharArray();

            // check for duplicates and get largest digit
            char largest = '0';
            for (var i = 0; i < digits.Length; i++)
            {
                var digit = digits[i];

                if (digit == '0')
                    return false;

                if (digit > largest)
                    largest = digit;

                for (var j = 0; j < digits.Length; j++)
                {
                    if (i != j && digit == digits[j])
                        return false;
                }
            }

            // verify the digits are 1 to n, no skipped values
            return (int)Char.GetNumericValue(largest) == digits.Length;
        }

        public string Solve()
        {
            var primes = new Primes((int)1e9);
            primes.CalculatePrimes();

            foreach (var prime in primes.PrimesListBackwards())
            {
                if (IsPandigital(prime))
                    return prime.ToString();
            }

            return "";
        }
    }
}
