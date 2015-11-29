namespace Fs

module Sequences =
    open System.Collections.Generic 

    // Memoize a sequence
    let Memoize s =
        let cache = List<_>()
        seq {
            yield! cache
            for x in s |> Seq.skip cache.Count do
                cache.Add(x)
                yield x
        }

    let Fibonacci =
        let generator = fun (a, b) ->
            let c = a+b in Some (c, (b, c))
        seq { yield 0; yield 1; yield! Seq.unfold generator (0, 1) }
        |> Memoize

    // A prime generator from 
    // http://stackoverflow.com/questions/4629734/the-sieve-of-eratosthenes-in-f
    let Primes = 

        let rec nextPrime n p primesMap =
            if primesMap |> Map.containsKey n then
                nextPrime (n + p) p primesMap
            else
                primesMap.Add(n, p)

        let rec prime n primesMap =
            seq {
                if primesMap |> Map.containsKey n then
                    let p = primesMap.Item n
                    yield! prime (n + 1L) (nextPrime (n + p) p (primesMap.Remove n))
                else
                    yield n
                    yield! prime (n + 1L) (primesMap.Add(n * n, n))
            }

        prime 2L Map.empty

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

    // todo: figure out the best memoization strategy in F# and use it (yes, state is helpful!)
    let TriangleNumbers = seq {
        let mutable current = 0
        for num in Seq.initInfinite id do
            current <- current + num
            yield current
    }
