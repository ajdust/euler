open System.Collections.Generic

type PrimeGenerator =
    val mutable n: int64
    val mutable last: int64
    val mutable sieve: Dictionary<int64, int64>

    new() = { n = 3L; last = 2L; sieve = Dictionary<int64, int64>() }

    member this.Next() =
        let mutable prime: int64 = 0L
        while this.sieve.TryGetValue(this.n, &prime) do
            this.sieve.Remove(this.n) |> ignore;

            let mutable composite = this.n + prime + prime
            while this.sieve.ContainsKey(composite) do
                composite <- composite + prime + prime
            this.sieve.Add(composite, prime)

            this.n <- this.n + 2L

        this.sieve.Add(this.n * this.n, this.n)
        let r = this.last
        this.last <- this.n
        this.n <- this.n + 2L
        r

type FactorFinder =
    val known: Dictionary<int64, HashSet<int64>>
    val nextPrimes: PrimeGenerator
    val knownPrimes: List<int64>

    new() = {
        known = (let k = Dictionary<int64, HashSet<int64>>()
                 let initKnown = HashSet<int64>()
                 initKnown.Add(1L) |> ignore
                 k.[1L] <- initKnown
                 k);
        nextPrimes = PrimeGenerator();
        knownPrimes = List<int64>();

    }

    member this.GetPrimeFactors(ofn: int64): List<int64> =
        let factors = List<int64>()
        let mutable quotient = ofn
        let mutable kprimes = this.knownPrimes.GetEnumerator()

        while kprimes.MoveNext() && kprimes.Current <= quotient do
            let mutable remainder = quotient % kprimes.Current
            while remainder = 0L do
                quotient <- quotient / kprimes.Current
                remainder <- quotient % kprimes.Current
                factors.Add(kprimes.Current)

        let mutable prime: int64 = this.nextPrimes.Next()
        this.knownPrimes.Add(prime)
        while prime <= quotient do

            let mutable remainder = quotient % prime
            while remainder = 0L do
                quotient <- quotient / prime
                remainder <- quotient % prime
                factors.Add(prime)

            prime <- this.nextPrimes.Next()
            this.knownPrimes.Add(prime)

        factors

    member this.GetFactors(ofn: int64): HashSet<int64> =
        match this.known.TryGetValue(ofn) with
        | (true, facs) -> facs
        | (false, _) ->
            let primeFactors = this.GetPrimeFactors(ofn)
            let factors = HashSet<int64>()
            factors.Add(1L) |> ignore;
            factors.Add(ofn) |> ignore;
            for prime in primeFactors do
                let factor = ofn / prime
                for subfactor in this.GetFactors(factor) do
                    factors.Add(subfactor) |> ignore;

            this.known.Add(ofn, factors)
            factors

let solve(): int64 =
    let finder = FactorFinder()
    let mutable adder = 0L
    let mutable tn = 0L
    let mutable factorCount = 0

    while factorCount < 1000 do
        adder <- adder + 1L
        tn <- tn + adder
        let tnFactors = finder.GetFactors tn
        let count = tnFactors.Count
        factorCount <- count

    tn

[<EntryPoint>]
let main args =
    let answer: int64 = solve()
    printfn "Answer: %i" answer
    0