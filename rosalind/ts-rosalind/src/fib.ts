
export function fib(n: number, k: number): BigInt {
    const kb = BigInt(k);
    let previous = 0n, current = 1n;
    for (let i = 0; i < n - 1; ++i) {
        const temp = current;
        current += previous * kb;
        previous = temp;
    }

    return current;
}
