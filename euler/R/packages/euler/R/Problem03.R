# Author: Aaron Johnson
# Date: 2016-09-04

# Largest prime factor
# The prime factors of 13195 are 5, 7, 13 and 29.
# What is the largest prime factor of the number 600851475143 ?

library(hash)

primes <- function() {
	# wrap hash library functions - only key strings are supported
	haskey <- function(k, m) has.key(as.character(k), m)
	setkey <- function(k, v, m) m[[as.character(k)]] <- v
	getkey <- function(k, m) m[[as.character(k)]]
	delkey <- function(k, m) delete(as.character(k), m)

	# sieve is passed by reference
	addNextComposite <- function(n, prime, sieve) {
		while (haskey(n, sieve)) n <- n + prime
		setkey(n, prime, sieve)
	}

	primeGen <- function(n, sieve) {
		# mimic tail-call recursion optimization with a while-loop
		while (haskey(n, sieve)) {
			prime <- getkey(n, sieve)

			delkey(n, sieve)
			addNextComposite(n + prime, prime, sieve)
			n <- n + 1
		}

		setkey(n*n, n, sieve)
		list(getnext=function() primeGen(n + 1, sieve),
			 current=n)
	}

	primeGen(2, hash())
}

primeFactors <- function(x) {
	pfacts <- c()
	quotient <- x
	remainder <- 0
	prime <- primes()
	p <- prime$current

	while (p <= quotient) {
		remainder <- quotient %% p
		while (remainder == 0) {
			quotient <- quotient %/% p
			remainder <- quotient %% p
			pfacts <- append(pfacts, p)
		}

		prime <- prime$getnext()
		p <- prime$current
	}

	pfacts
}

problem03 <- function() {
	max(primeFactors(600851475143))
}