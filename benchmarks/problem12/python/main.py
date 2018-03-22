# run with: python3 main.py

import array

class PrimeGenerator:
    n = 3
    last = 2
    sieve = {}

    def next(self):
        prime = self.sieve.get(self.n, 0)
        while prime != 0:
            del self.sieve[self.n]

            composite = self.n + prime + prime
            while composite in self.sieve:
                composite += prime + prime
            self.sieve[composite] = prime

            self.n += 2

            prime = self.sieve.get(self.n, 0)

        self.sieve[self.n*self.n] = self.n
        r = self.last
        self.last = self.n
        self.n += 2

        return r

class FactorFinder:
    known = { 1: set([1]) }
    nextPrimes = PrimeGenerator()
    knownPrimes = array.array('l')

    def getPrimeFactors(self, of):
        factors = []
        quotient = of

        for prime in self.knownPrimes:
            if prime > quotient:
                break

            remainder = quotient % prime
            while remainder == 0:
                quotient = quotient // prime
                remainder = quotient % prime
                factors.append(prime)

        while True:
            prime = self.nextPrimes.next()
            self.knownPrimes.append(prime)

            if prime > quotient:
                break

            remainder = quotient % prime
            while remainder == 0:
                quotient = quotient // prime
                remainder = quotient % prime
                factors.append(prime)

        return factors

    def getFactors(self, of):
        existing = self.known.get(of)
        if existing is not None:
            return existing

        factors = set([1, of])
        for prime in self.getPrimeFactors(of):
            factor = of // prime
            for subfactor in self.getFactors(factor):
                factors.add(subfactor)

        self.known[of] = factors
        return factors

def solve():
    finder = FactorFinder()
    adder = 0
    tn = 0

    while True:
        adder += 1
        tn += adder
        factors = finder.getFactors(tn)
        if len(factors) > 500:
            return tn

def main():
    answer = solve()
    print("Answer: " + str(answer))

if __name__ == "__main__":
    main()






