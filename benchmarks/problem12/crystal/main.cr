ZERO  = 0.to_i64
ONE   = 1.to_i64
TWO   = 2.to_i64
THREE = 3.to_i64

class PrimeGenerator
  @n : Int64 = THREE
  @last : Int64 = TWO
  @sieve = {} of Int64 => Int64

  def next
    prime = @sieve[@n]?
    while true
      if prime
        @sieve.delete(@n)

        composite = @n + prime + prime
        while @sieve.has_key?(composite)
          composite += prime + prime
        end
        @sieve[composite] = prime

        @n += TWO
        prime = @sieve[@n]?
      else
        break
      end
    end

    @sieve[@n * @n] = @n
    r = @last
    @last = @n
    @n += TWO
    r
  end
end

class FactorFinder
  @known = {ONE => Set{ONE}} of Int64 => Set(Int64)
  @nextPrimes = PrimeGenerator.new
  @knownPrimes = [] of Int64

  def get_prime_factors(ofn : Int64)
    factors = [] of Int64
    quotient = ofn

    @knownPrimes.each { |prime|
      if prime > quotient
        return factors
      end

      remainder = quotient % prime
      while remainder == ZERO
        quotient = quotient / prime
        remainder = quotient % prime
        factors << prime
      end
    }

    while true
      prime = @nextPrimes.next
      @knownPrimes << prime

      if prime > quotient
        return factors
      end

      remainder = quotient % prime
      while remainder == ZERO
        quotient = quotient / prime
        remainder = quotient % prime
        factors << prime
      end
    end

    factors
  end

  def get_factors(ofn : Int64) : Set(Int64)
    existing = @known[ofn]?
    if existing
      return existing
    end

    factors = Set{ONE, ofn}
    get_prime_factors(ofn).each { |prime|
      factor = ofn / prime
      get_factors(factor).each { |subfactor|
        factors << subfactor
      }
    }

    @known[ofn] = factors
    factors
  end
end

def solve
  finder = FactorFinder.new
  adder = ZERO
  tn = ZERO

  while true
    adder += ONE
    tn += adder

    factors = finder.get_factors(tn)
    if factors.size > 1000
      return tn
    end
  end
end

answer = solve()
puts "Answer: #{answer}"
