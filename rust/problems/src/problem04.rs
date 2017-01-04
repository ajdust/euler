/// Aaron Johnson
/// 2017-01-03

/// Largest palindromic product
/// A palindromic number reads the same both ways. The largest palindrome
/// made from the product of two 2-digit numbers is 9009 = 91 × 99.
/// Find the largest palindrome made from the product of two 3-digit numbers.

fn is_palindrome(num: i64) -> bool {
    let num_string = num.to_string();
    let half = num_string.len() / 2;

    num_string.bytes().take(half).eq(num_string.bytes().rev().take(half))
}

pub fn solve() -> i64 {
    match (100..999)
        .flat_map(move |x| (100..x).map(move |y| x*y))
        .filter(|x| is_palindrome(*x)).max() {
        Some(v) => v,
        _ => 0
    }
}