
class PrimeGenerator
{
    constructor() {
        this.n = 3;
        this.last = 2;
        this.sieve = new Map();
    }

    next() {
        let prime = 0;
        while (this.sieve.has(this.n))
        {
            prime = this.sieve.get(this.n);

            this.sieve.delete(this.n);

            let composite = this.n + prime + prime;
            while (this.sieve.has(composite))
                composite += prime + prime;
            this.sieve.set(composite, prime);

            this.n = this.n + 2;
        }

        this.sieve.set(this.n * this.n, this.n);
        const r = this.last;
        this.last = this.n;
        this.n += 2;
        return r;
    }
}

class FactorFinder
{
    constructor() {
        this.known = new Map();
        this.nextPrimes = new PrimeGenerator();
        this.knownPrimes = [];
        const init = new Set();
        init.add(1);
        this.known.set(1, init);
    }

    getPrimeFactors(of)
    {
        const factors = [];
        let quotient = of;

        for (let prime of this.knownPrimes)
        {
            if (prime > quotient)
                return factors;

            let remainder = quotient % prime;
            while (remainder == 0)
            {
                quotient /= prime;
                remainder = quotient % prime;
                factors.push(prime);
            }
        }

        for (;;)
        {
            const prime = this.nextPrimes.next();
            this.knownPrimes.push(prime);

            if (prime > quotient)
                return factors;

            let remainder = quotient % prime;
            while (remainder == 0)
            {
                quotient /= prime;
                remainder = quotient % prime;
                factors.push(prime);
            }
        }
    }

    getFactors(of)
    {
        if (this.known.has(of))
            return this.known.get(of);

        const factors = new Set();
        factors.add(1);
        factors.add(of);

        for (const prime of this.getPrimeFactors(of))
        {
            const factor = of / prime;
            for (const subfactor of this.getFactors(factor))
            {
                factors.add(subfactor);
            }
        }

        this.known.set(of, factors);
        return factors;
    }
}

function solve()
{
    const finder = new FactorFinder();
    let adder = 0;
    let tn = 0;

    for (;;)
    {
        adder += 1;
        tn += adder;
        const factors = finder.getFactors(tn);
        if (factors.size > 1000)
            return tn;
    }
}

const answer = solve();
console.log(`Answer: ${answer}`);