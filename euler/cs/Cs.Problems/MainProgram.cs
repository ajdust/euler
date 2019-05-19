using System;

namespace Cs.Problems
{
    class MainProgram
    {
        static void Main(string[] args)
        {
            // var test = new Primes(128);
            // test.CalculatePrimes();
            var p = new Problem012();
            Console.WriteLine(p.Solve());
        }
    }
}
