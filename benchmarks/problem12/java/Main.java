import java.util.*;

class PrimeGenerator
{
    Long n = 3L;
    Long last = 2L;
    HashMap<Long, Long> sieve = new HashMap<Long, Long>();

    public Long Next()
    {
        Long prime = sieve.getOrDefault(n, 0L);
        while (prime != 0L)
        {
            sieve.remove(n);

            Long composite = n + prime + prime;
            while (sieve.containsKey(composite))
                composite += prime + prime;
            sieve.put(composite, prime);

            n = n + 2L;
            prime = sieve.getOrDefault(n, 0L);
        }

        sieve.put(n * n, n);
        Long r = last;
        last = n;
        n += 2L;
        return r;
    }
}

class FactorFinder
{
    HashMap<Long, HashSet<Long>> known = new HashMap<Long, HashSet<Long>>();
    PrimeGenerator nextPrimes = new PrimeGenerator();
    ArrayList<Long> knownPrimes = new ArrayList<Long>();

    public FactorFinder()
    {
        HashSet<Long> one = new HashSet<Long>();
        one.add(1L);
        known.put(1L, one);
    }

    public ArrayList<Long> getPrimeFactors(Long of)
    {
        ArrayList<Long> factors = new ArrayList<Long>();
        Long quotient = of;

        for (Long prime : knownPrimes)
        {
            if (prime > quotient)
                return factors;

            Long remainder = quotient % prime;
            while (remainder == 0L)
            {
                quotient /= prime;
                remainder = quotient % prime;
                factors.add(prime);
            }
        }

        for (;;)
        {
            Long prime = nextPrimes.Next();
            knownPrimes.add(prime);

            if (prime > quotient)
                return factors;

            Long remainder = quotient % prime;
            while (remainder == 0L)
            {
                quotient /= prime;
                remainder = quotient % prime;
                factors.add(prime);
            }
        }
    }

    public HashSet<Long> getFactors(Long of)
    {
        HashSet<Long> factors = known.get(of);
        if (factors != null)
            return factors;

        factors = new HashSet<Long>();
        factors.add(1L);
        factors.add(of);

        for (Long prime : getPrimeFactors(of))
        {
            Long factor = of / prime;
            for (Long subfactor : getFactors(factor))
            {
                factors.add(subfactor);
            }
        }

        known.put(of, factors);
        return factors;
    }
}

public class Main
{
    static Long solve()
    {
        FactorFinder finder = new FactorFinder();
        Long adder = 0L;
        Long tn = 0L;

        for (;;)
        {
            adder += 1;
            tn += adder;

            HashSet<Long> factors = finder.getFactors(tn);
            if (factors.size() > 1000)
                return tn;
        }
    }

    public static void main(String[] args)
    {
        Long answer = solve();
        System.out.print("Answer: " + answer.toString() + "\n");
    }
}