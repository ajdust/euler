/// Aaron Johnson
/// 2017-01-09

/// A Pythagorean triplet is a set of three natural numbers, a LT b LT c, for which,
/// a^2 + b^2 = c^2
///
/// For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
///
/// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
/// Find the product a*b*c.

pub fn solve() -> i32 {
    let answers =
        (1..999).flat_map(move |c|
        (1..c).flat_map(move |b|
        (1..b).filter_map(move |a|
            if a*a + b*b == c*c && a + b + c == 1000 {
                Some((c, b, a))
            } else {
                None
            })));

    match answers.collect::<Vec<(i32,i32,i32)>>().first() {
        Some(answer) => answer.0 * answer.1 * answer.2,
        None => 0
    }
}