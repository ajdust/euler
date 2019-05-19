/// Aaron Johnson
/// 2017-01-09

/// 10001st prime
///
/// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
/// we can see that the 6th prime is 13. What is the 10001st prime number?

use problem03::Primes;

pub fn solve() -> i64 {
    match Primes::new().nth(10000) {
        Some(v) => v,
        None => 0
    }
}