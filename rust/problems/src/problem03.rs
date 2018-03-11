/// Aaron Johnson
/// 2016-11-13

/// Largest prime factor
///
/// The prime factors of 13195 are 5, 7, 13 and 29.
/// What is the largest prime factor of the number 600851475143 ?

use std::collections::HashMap;

pub struct Primes {
    n: i64,
    last: i64,
    sieve: HashMap<i64, i64>
}

impl Primes {
    pub fn new() -> Primes {
        Primes { n: 3, last: 2, sieve: HashMap::new() }
    }
}

impl Iterator for Primes {
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
            let mut n_ = self.n + prime + prime;
            while self.sieve.contains_key(&n_) {
                n_ += prime + prime;
            }
            self.sieve.insert(n_, prime);

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

pub fn prime_factors(of: i64) -> Vec<i64> {
    let mut pfacts = Vec::new();
    let mut quotient = of;

    for prime in Primes::new() {
        if prime > quotient {
            break;
        }

        let mut remainder = quotient % prime;
        while remainder == 0 {
            quotient = quotient / prime;
            remainder = quotient % prime;
            pfacts.push(prime);
        }
    }

    pfacts
}

pub fn solve() -> i64 {
    // let primes = Primes::new().take(10).collect::<Vec<i64>>();
    // println!("Got: {:?}", primes);
    // let pfacts = prime_factors(20);
    // println!("Got pf: {:?}", pfacts);

    // there is a nicer way to get the max..
    let pf = prime_factors(600851475143);
    let mut m = 0;
    for v in pf {
        if v > m {
            m = v;
        }
    }

    m
}