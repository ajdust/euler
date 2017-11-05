
namespace Problems


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
        let rec addNextComposite n prime sieve =
            match Map.containsKey n sieve with
            | true -> addNextComposite (n + prime + prime) prime sieve
            | false -> Map.add n prime sieve
        let rec primes n sieve =
            seq {
                match Map.tryFind n sieve with
                | Some prime ->
                    let gc = Map.remove n sieve
                    let ac = addNextComposite (n + prime + prime) prime gc
                    in yield! primes (n + 2L) ac
                | None ->
                    let ac = Map.add (n * n) n sieve
                    yield n
                    yield! primes (n + 2L) ac
            }
        in seq {
            yield 2L
            yield! primes 3L Map.empty
        }

    let PrimeFactors n =
        let rec pfactors primes n =
            seq {
                match Seq.tryHead primes with
                | None -> failwith "No next prime"
                | Some prime ->
                    if prime >= n then
                        yield prime
                    elif n % prime = 0L then
                        yield prime
                        yield! pfactors primes (n / prime)
                    else
                        yield! pfactors (Seq.skip 1 primes) n
            }
        in pfactors (Primes ()) n

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

