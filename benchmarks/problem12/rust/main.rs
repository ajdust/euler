use std::collections::HashMap;
use std::collections::HashSet;

pub struct PrimeGenerator {
    n: i64,
    last: i64,
    sieve: HashMap<i64, i64>
}

impl PrimeGenerator {
    pub fn new() -> PrimeGenerator {
        PrimeGenerator { n: 3, last: 2, sieve: HashMap::new() }
    }
}

impl Iterator for PrimeGenerator {
    type Item = i64;
    fn next(&mut self) -> Option<Self::Item> {

        loop {

            let prime: i64;
            {
                if let Some(prime_) = self.sieve.get(&self.n) {
                    prime = *prime_;
                } else {
                    break;
                }
            }

            // don't need this key - remove it to save space
            self.sieve.remove(&self.n);

            // add composite
            let mut composite = self.n + prime + prime;
            while self.sieve.contains_key(&composite) {
                composite += prime + prime;
            }
            self.sieve.insert(composite, prime);

            self.n += 2;
        }

        // add composite in prep for next round
        self.sieve.insert(self.n * self.n, self.n);
        let r = Some(self.last);
        self.last = self.n;
        self.n += 2;
        r
    }
}

struct FactorFinder {
    known: HashMap<i64, HashSet<i64>>,
    next_primes: PrimeGenerator,
    known_primes: Vec<i64>
}

impl FactorFinder {

    pub fn new() -> FactorFinder {
        let mut known: HashMap<i64, HashSet<i64>> = HashMap::new();
        let mut one: HashSet<i64> = HashSet::new();
        one.insert(1);
        known.insert(1, one);
        FactorFinder {
            known: known,
            next_primes: PrimeGenerator::new(),
            known_primes: Vec::new()
        }
    }

    pub fn get_prime_factors(&mut self, of: i64) -> Vec<i64> {
        let mut factors = Vec::new();
        let mut quotient = of;

        {
            for prime_ in self.known_primes.iter() {
                let prime = *prime_;
                if prime > quotient {
                    return factors;
                }

                let mut remainder = quotient % prime;
                while remainder == 0 {
                    quotient = quotient / prime;
                    remainder = quotient % prime;
                    factors.push(prime);
                }
            }
        }

        {
            loop {
                let prime: i64;
                {
                    if let Some(prime_) = self.next_primes.next() {
                        prime = prime_;
                    } else {
                        break;
                    }
                }

                self.known_primes.push(prime);
                if prime > quotient {
                    return factors;
                }

                let mut remainder = quotient % prime;
                while remainder == 0 {
                    quotient = quotient / prime;
                    remainder = quotient % prime;
                    factors.push(prime);
                }
            }
        }

        factors
    }

    pub fn get_factors(&mut self, n: i64) -> HashSet<i64> {

        {
            if let Some(factors) = self.known.get(&n) {
                return (*factors).clone()
            }
        }

        {
            let pfs = self.get_prime_factors(n);
            let mut factor_set: HashSet<i64> = HashSet::new();
            factor_set.insert(1);
            factor_set.insert(n);

            for prime in pfs {
                let factor = n / prime;
                for subfactor in self.get_factors(factor) {
                    factor_set.insert(subfactor);
                }
            }

            self.known.insert(n, factor_set.clone());
            return factor_set
        }

    }
}

pub fn solve() -> i64 {

    let mut finder = FactorFinder::new();

    let mut adder: i64 = 0;
    let mut tn: i64 = 0;

    loop {
        adder += 1;
        tn += adder;

        let tn_factors = finder.get_factors(tn);
        if tn_factors.len() > 1000 {
            return tn
        }
    }

}

pub fn main() {
    let answer = solve();
    println!("Answer: {}", answer);
}