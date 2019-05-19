
namespace Problems

open System.Collections.Generic

module Sequences =
    let Fibonacci () = seq {
        let mutable a = 0
        let mutable b = 1
        while true do
            let temp = b
            b <- a + b
            a <- temp
            yield b
    }
    let Primes () =
        let mutable n = 3L
        let composites = new Dictionary<int64, int64>()
        in seq {
            yield 2L
            while true do
                while composites.ContainsKey(n) do
                    let prime = composites.[n]
                    composites.Remove(n) |> ignore;
                    let mutable check = n + prime + prime
                    while composites.ContainsKey(check) do
                        check <- check + prime + prime
                    composites.Add(check, prime)
                    n <- n + 2L
                composites.[n*n] <- n
                yield n
                n <- n + 2L
        }

    let PrimeFactors n =
        let factors = new List<int64>()
        let primes = (Primes ()).GetEnumerator()
        primes.MoveNext() |> ignore;
        let mutable quotient = n
        while primes.Current <= quotient do
            let mutable remainder = quotient % primes.Current
            while remainder = 0L do
                quotient <- quotient / primes.Current
                remainder <- quotient % primes.Current
                factors.Add(primes.Current)
            primes.MoveNext() |> ignore
        factors

    let Squares = Seq.initInfinite (fun i -> (i, i * i))

    let PythagoreanTriplets () =
        seq { for (c, c2) in Squares do
              for (b, b2) in Seq.takeWhile (fun (b, _) -> b < c) Squares do
              for (a, a2) in Seq.takeWhile (fun (a, _) -> a < b) Squares do
              if a2 + b2 = c2 then
                  yield (a, b, c) }

    let TriangleNumbers () = seq {
        let mutable currentN = 1L
        let mutable currentTotal = 0L
        while true do
            currentTotal <- currentTotal + currentN
            currentN <- currentN + 1L
            yield currentTotal
    }

