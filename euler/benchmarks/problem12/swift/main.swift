
class PrimeGenerator {
    var n = 3
    var last = 2
    var sieve = [Int: Int]()

    func next() -> Int {
        while let prime = sieve[n] {
            sieve[n] = nil

            var composite: Int = n + prime + prime
            while sieve[composite] != nil {
                composite += prime + prime
            }

            sieve[composite] = prime
            n += 2
        }

        sieve[n*n] = n
        let r = last
        last = n
        n += 2
        return r
    }
}

class FactorFinder {
    var known: [Int: Set<Int>] = [1: [1]]
    var nextPrimes = PrimeGenerator()
    var knownPrimes: [Int] = [Int]()

    func getPrimeFactors(of: Int) -> [Int] {
        var factors = [Int]()
        var quotient = of

        for prime in knownPrimes {
            if prime > quotient {
                return factors
            }

            var remainder = quotient % prime
            while remainder == 0 {
                quotient /= prime
                remainder = quotient % prime
                factors.append(prime)
            }
        }

        while true {
            let prime = nextPrimes.next()
            knownPrimes.append(prime)

            if prime > quotient {
                return factors
            }

            var remainder = quotient % prime
            while (remainder == 0) {
                quotient /= prime
                remainder = quotient % prime
                factors.append(prime)
            }
        }
    }

    func getFactors(of: Int) -> Set<Int> {

        if let existing = known[of] {
            return existing
        }

        let primeFactors = getPrimeFactors(of: of)
        var factors: Set = [1, of]
        for prime in primeFactors {
            let factor = of / prime
            for subfactor in getFactors(of: factor) {
                factors.insert(subfactor)
            }
        }

        known[of] = factors
        return factors
    }
}

func solve() -> Int {
    let finder = FactorFinder()
    var adder = 0
    var tn = 0
    var length = 0

    while length < 1000 {
        adder += 1
        tn += adder

        let factors = finder.getFactors(of: tn)
        length = factors.count
    }

    return tn
}

print("Running")
let answer = solve()
print("Answer: \(answer)")

