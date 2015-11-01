using System.Linq;

namespace euler
{
    /// <title>10001st prime</title>
    /// <summary>
    /// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
    /// What is the 10001st prime number?
    /// </summary>
    /// <answer>104743</answer>
    public class Problem007 : IProblem
    {
        static long Solve(int n)
        {
            // finding the nth prime, reusing Problem 3's prime enumerable,
            // which while not the most efficient prime finding algorithm, works reasonably
            // well for finding only the prime number 10001
            return Sequences.Primes().Skip(n).First();
        }
        public string Solve()
        {
            return Solve(10000).ToString();
        }
    }
}
