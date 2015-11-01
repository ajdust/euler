using System.Linq;

namespace euler
{
    /// <title>Summation of primes</title>
    ///  <summary>
    /// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    /// Find the sum of all the primes below two million.
    /// </summary>
    /// <remarks>
    /// This problem calls for a more efficient prime computing algorithm than that of Problem 3.
    /// Hence, the Primes class was created here to use the Sieve of Eratosthenes on a bitmap.
    /// Ideally, this would call out to some sweet multithreaded C++ DLL to do the dirty work for us.
    /// But with a limit of only 2,000,000 C# can do the work.
    /// </remarks>
    /// <answer>142913828922</answer>
    public class Problem010 : IProblem
    {
        public string Solve()
        {
            var p = new Primes(2000000);
            p.CalculatePrimes();
            long s = p.PrimeList().Select(x => (long)x).Sum();
            return s.ToString();
        }
    }
}
