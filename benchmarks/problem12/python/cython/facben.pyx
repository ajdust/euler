#distutils: language = c++

from libcpp.unordered_map cimport unordered_map
from libcpp.pair cimport pair
from libcpp.unordered_set cimport unordered_set
from libcpp.vector cimport vector
from cython.operator cimport dereference as deref

cdef class PrimeGenerator:
    cdef long n
    cdef long last
    cdef unordered_map[long, long] sieve

    def __init__(self):
        self.n = 3
        self.last = 2

    cdef next(self):
        cdef long prime
        cdef long composite
        cdef long r

        kv = self.sieve.find(self.n)
        while kv != self.sieve.end():
            self.sieve.erase(kv)
            prime = deref(kv).second

            composite = self.n + prime + prime
            while self.sieve.find(composite) != self.sieve.end():
                composite += prime + prime
            self.sieve[composite] = prime

            self.n += 2

            kv = self.sieve.find(self.n)

        self.sieve[self.n*self.n] = self.n
        r = self.last
        self.last = self.n
        self.n += 2

        return r

cdef class FactorFinder:
    cdef unordered_map[long, unordered_set[long]] known
    cdef PrimeGenerator nextPrimes
    cdef vector[long] knownPrimes

    def __init__(self):

        # self.known = { 1: set([1]) }
        cdef unordered_set[long] one
        one.insert(1)
        cdef pair[long, unordered_set[long]] justone
        justone.first = 1
        justone.second = one
        self.known.insert(justone)
        self.nextPrimes = PrimeGenerator()

    cdef getPrimeFactors(self, long of):
        cdef vector[long] factors
        cdef long quotient = of

        for prime in self.knownPrimes:
            if prime > quotient:
                break

            remainder = quotient % prime
            while remainder == 0:
                quotient = quotient // prime
                remainder = quotient % prime
                factors.push_back(prime)

        while True:
            prime = self.nextPrimes.next()
            self.knownPrimes.push_back(prime)

            if prime > quotient:
                break

            remainder = quotient % prime
            while remainder == 0:
                quotient = quotient // prime
                remainder = quotient % prime
                factors.push_back(prime)

        return factors

    cdef getFactors(self, long of):
        existing = self.known.find(of)
        if existing != self.known.end():
            return deref(existing).second

        # factors = set([1, of])
        cdef unordered_set[long] factors
        factors.insert(1)
        factors.insert(of)

        for prime in self.getPrimeFactors(of):
            factor = of // prime
            for subfactor in self.getFactors(factor):
                factors.insert(subfactor)

        cdef pair[long, unordered_set[long]] temp
        temp.first = of
        temp.second = factors
        self.known.insert(temp)
        return factors

def solve():
    finder = FactorFinder()
    cdef long adder = 0
    cdef long tn = 0

    while True:
        adder += 1
        tn += adder
        factors = finder.getFactors(tn)
        if len(factors) > 500:
            return tn







