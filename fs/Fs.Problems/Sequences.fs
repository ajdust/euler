namespace Fs
module Sequences =

    (* Original imperative Fibonacci solution

    let Fibonacci = seq {
        let mutable a = 0
        let mutable b = 1
        yield a
        yield b
        while true do
            let temp = a + b
            a <- b
            b <- temp
            yield b
    }

    *)

    // This solution appears to be the most elegant, though not as readable, and not as inspiring as Haskell's lazy Fibonacci definition
    let Fibonacci = Seq.unfold (fun (a, b) -> Some (a+b, (b, a+b)) ) (0, 1)

    // A simple, inefficient primes sequence
    let Primes = seq {
        let mutable primes = [2L]
        yield 2L
        yield! seq {
            for x in seq { 3L .. 2L .. System.Int64.MaxValue } do
                if Seq.forall (fun p -> x % p <> 0L) primes then
                    primes <- x :: primes
                    yield x
        }
    }

    let PrimeFactors x = seq {
        let mutable quotient = x
        let mutable remainder = 0L
        // for each prime, see how many times you can divide the quotient with it
        yield! seq {
            for prime in Seq.takeWhile (fun p -> p <= quotient) Primes do
                remainder <- quotient % prime
                while remainder = 0L do
                    quotient <- quotient / prime
                    remainder <- quotient % prime
                    yield prime
        }
    }

    let Squares = Seq.initInfinite (fun i -> (i, i*i))

    let PythagoreanTriplets = seq {
        for (c, c2) in Squares do
        for (b, b2) in Seq.takeWhile (fun (b, _) -> b < c) Squares do
        for (a, a2) in Seq.takeWhile (fun (a, _) -> a < b) Squares do
        if a2 + b2 = c2 then
            yield (a, b, c)
    }

    // A more efficient prime generator from 
    // http://stackoverflow.com/questions/4629734/the-sieve-of-eratosthenes-in-f
    // Let the prime experts handle the crazy functional prime algorithm, eh?
    let (bigPrimes : seq<int64>) = 
        let rec nextPrime n p primes =
            if primes |> Map.containsKey n then
                nextPrime (n + p) p primes
            else
                primes.Add(n, p)

        let rec prime n primes =
            seq {
                if primes |> Map.containsKey n then
                    let p = primes.Item n
                    yield! prime (n + 1L) (nextPrime (n + p) p (primes.Remove n))
                else
                    yield n
                    yield! prime (n + 1L) (primes.Add(n * n, n))
            }

        prime 2L Map.empty