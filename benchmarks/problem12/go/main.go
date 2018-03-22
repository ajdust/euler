package main

import (
	"fmt"
	"strconv"
)

var _ = fmt.Printf
var _ = strconv.FormatInt

type PrimeGenerator struct {
	n, last int64
	sieve   map[int64]int64
}

func MakePrimeGenerator() *PrimeGenerator {
	return &PrimeGenerator{3, 2, make(map[int64]int64)}
}

func (self *PrimeGenerator) Next() int64 {
	prime, ok := self.sieve[self.n]
	for ok {
		delete(self.sieve, self.n)

		var composite int64 = self.n + prime + prime
		_, ok_ := self.sieve[composite]
		for ok_ {
			composite += prime + prime
			_, ok_ = self.sieve[composite]
		}
		self.sieve[composite] = prime

		self.n += 2
		prime, ok = self.sieve[self.n]
	}

	self.sieve[self.n*self.n] = self.n
	r := self.last
	self.last = self.n
	self.n += 2
	return r
}

type FactorFinder struct {
	known       map[int64][]int64
	nextPrimes  *PrimeGenerator
	knownPrimes []int64
}

func MakeFactorFinder() *FactorFinder {
	known := make(map[int64][]int64)
	known[1] = []int64{1}
	return &FactorFinder{known, MakePrimeGenerator(), make([]int64, 0)}
}

func (self *FactorFinder) GetPrimeFactors(of int64) []int64 {
	factors := make([]int64, 0)
	quotient := of

	for _, prime := range self.knownPrimes {
		if prime > quotient {
			return factors
		}

		remainder := quotient % prime
		for remainder == 0 {
			quotient = quotient / prime
			remainder = quotient % prime
			factors = append(factors, prime)
		}
	}

	for {
		prime := self.nextPrimes.Next()
		self.knownPrimes = append(self.knownPrimes, prime)
		if prime > quotient {
			return factors
		}

		remainder := quotient % prime
		for remainder == 0 {
			quotient = quotient / prime
			remainder = quotient % prime
			factors = append(factors, prime)
		}
	}

	return factors
}

func (self *FactorFinder) GetFactors(of int64) []int64 {

	if facs, ok := self.known[of]; ok {
		return facs
	}

	primeFactors := self.GetPrimeFactors(of)
	factorSet := make(map[int64]bool)
	factorSet[1] = true
	factorSet[of] = true

	for _, prime := range primeFactors {

		factor := of / prime
		for _, subfactor := range self.GetFactors(factor) {
			factorSet[subfactor] = true
		}
	}

	keys := make([]int64, len(factorSet))
	i := 0
	for k := range factorSet {
		keys[i] = k
		i++
	}

	self.known[of] = keys
	return keys
}

func Solve() int64 {

	finder := MakeFactorFinder()
	var adder int64 = 0
	var tn int64 = 0

	for {
		adder += 1
		tn += adder
		tnFactors := finder.GetFactors(tn)

		if len(tnFactors) > 1000 {
			return tn
		}
	}
}

func main() {
	answer := Solve()
	println("Answer: ", answer)
}
