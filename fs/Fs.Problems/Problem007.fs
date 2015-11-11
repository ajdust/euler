namespace Fs

    /// <title>10001st prime</title>
    /// <summary>
    /// By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
    /// What is the 10001st prime number?
    /// </summary>

    type Problem007 () =

        let NumberSolve n = Seq.head (Seq.skip n Sequences.Primes)

        member this.Solve () = NumberSolve 10000 |> string
        interface IProblem with
            member this.Solve () = this.Solve ()

