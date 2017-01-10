/// Aaron Johnson
/// 2017-01-09

/// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
/// Find the sum of all the primes below two million.

use problem03::Primes;

pub fn solve() -> i64 {
    Primes::new().take_while(|x| *x < 2000000).sum()
}