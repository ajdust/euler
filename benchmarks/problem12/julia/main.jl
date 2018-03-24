# run with julia main.jl
# julia version used: 0.6.2

mutable struct PrimeGenerator
    n::Int64
    lastn::Int64
    sieve::Dict{Int64,Int64}
    PrimeGenerator() = new(3, 2, Dict())
end

function Base.start(::PrimeGenerator)
    PrimeGenerator()
end

function Base.next(self::PrimeGenerator, state)
    prime = get(self.sieve, self.n, 0)
    while prime != 0

        delete!(self.sieve, self.n)

        composite = self.n + prime + prime
        while haskey(self.sieve, composite)
            composite += prime + prime
        end
        self.sieve[composite] = prime

        self.n += 2
        prime = get(self.sieve, self.n, 0)
    end

    self.sieve[self.n * self.n] = self.n
    r = self.lastn
    self.lastn = self.n
    self.n += 2
    (self, r)
end

function Base.done(P::PrimeGenerator, self)
    false
end

struct FactorFinder
    known::Dict{Int64,Set{Int64}}
    nextPrimes::PrimeGenerator
    knownPrimes::Array{Int64}
    FactorFinder() = new(Dict(1 => Set(1)), PrimeGenerator(), [])
end

function getPrimeFactors(self::FactorFinder, of::Int64)
    factors = []
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
        (_, prime) = next(self.nextPrimes, 0)
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

function getFactors(self::FactorFinder, of::Int64)
    existing = get(self.known, of, 0)
    if existing != 0
        return existing
    end

    factors = Set([1, of])
    for prime in getPrimeFactors(self, of)
        factor = div(of, prime)
        for subfactor in getFactors(self, factor)
            push!(factors, subfactor)
        end
    end

    self.known[of] = factors
    factors
end

function solve()
    finder = FactorFinder()
    adder = 0
    tn = 0

    while true
        adder += 1
        tn += adder

        factors = getFactors(finder, tn)
        if length(factors) > 1000
            return tn
        end
    end
end

answer = solve()
println("Answer: " * string(answer))