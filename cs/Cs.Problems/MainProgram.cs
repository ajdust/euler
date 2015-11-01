using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace euler
{
    class MainProgram
    {
        static void Main(string[] args)
        {
            var test = new Primes(128);
            test.CalculatePrimes();
        }
    }
}
