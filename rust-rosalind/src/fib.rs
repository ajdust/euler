pub fn fib(n: u64, k: u64) -> u64 {
    let mut previous = 0;
    let mut current = 1;
    for _ in 0..n-1 {
        let temp = current;
        current += previous * k;
        previous = temp;
    }

    current
}