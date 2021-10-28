# run with julia main.jl
# julia version used: 0.6.3
module BenchmarkProblem12

mutable struct PrimeGenerator
    n::Int64
    lastn::Int64
    sieve::Dict{Int64,Int64}
    PrimeGenerator() = new(3, 2, Dict())
end

function nextPrime(self::PrimeGenerator)::Int64

    while haskey(self.sieve, self.n)
        prime = self.sieve[self.n]

        delete!(self.sieve, self.n)

        composite = self.n + prime + prime
        while haskey(self.sieve, composite)
            composite += prime + prime
        end
        self.sieve[composite] = prime

        self.n += 2
    end

    self.sieve[self.n * self.n] = self.n
    r = self.lastn
    self.lastn = self.n
    self.n += 2
    r
end

struct FactorFinder
    known::Dict{Int64,Set{Int64}}
    nextPrimes::PrimeGenerator
    knownPrimes::Vector{Int64}
    FactorFinder() = new(Dict(1 => Set(1)), PrimeGenerator(), [])
end

function getPrimeFactors(self::FactorFinder, of::Int64)::Vector{Int64}
    factors = Int64[]
    quotient = of

    for prime in self.knownPrimes
        if prime > quotient
            return factors
        end

        remainder = rem(quotient, prime)
        while remainder == 0
            quotient = div(quotient, prime)
            remainder = rem(quotient, prime)
            push!(factors, prime)
        end
    end

    while true
        prime = nextPrime(self.nextPrimes)
        push!(self.knownPrimes, prime)

        if prime > quotient
            return factors
        end

        remainder = rem(quotient, prime)
        while remainder == 0
            quotient = div(quotient, prime)
            remainder = rem(quotient, prime)
            push!(factors, prime)
        end
    end

    factors
end

function getFactors(self::FactorFinder, of::Int64)::Set{Int64}
    if haskey(self.known, of)
        return self.known[of]
    end

    factors = Set{Int64}([1, of])
    for prime in getPrimeFactors(self, of)
        factor = div(of, prime)
        for subfactor in getFactors(self, factor)
            push!(factors, subfactor)
        end
    end

    self.known[of] = factors
    factors
end

function solve(factorCount)::Int64
    finder = FactorFinder()
    adder = 0
    tn = 0

    while true
        adder += 1
        tn += adder

        factors = getFactors(finder, tn)
        if length(factors) > factorCount
            return tn
        end
    end
end

function main()
    println("Answer: " * string(solve(1000)))
    return 0
end

end