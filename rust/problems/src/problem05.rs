/// Aaron Johnson
/// 2016-01-05

/// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
/// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

use problem03::prime_factors;
use std::collections::HashMap;
use std::cmp::max;

fn collect_common_factors(numbers: Vec<i64>) -> HashMap<i64, i64> {
    let mut max_counts: HashMap<i64,i64> = HashMap::new();

    for n in numbers {
        // get prime factors
        let pfs: Vec<i64> = prime_factors(n);

        // count the number of each
        let mut counts: HashMap<i64,i64> = HashMap::new();
        for pf in pfs {
            let count = counts.entry(pf).or_insert(0);
            *count += 1;
        }

        // merge the results into max_counts
        for (key, val) in counts.iter() {
            let count = max_counts.entry(*key).or_insert(0);
            *count = max(*count, *val)
        }
    }

    max_counts
}

pub fn solve() -> i64 {
    let nums: Vec<i64> = (1..21).collect();
    let max_counts = collect_common_factors(nums);
    let mut prod: i64 = 1;
    for (key, val) in max_counts.iter() {
        prod *= (*key).pow(*val as u32);
    }

    prod
}