(*

Author: Aaron Johnson
Date:   2015-07-19

Solves Euler Problem 10.

*)
namespace Fs

    /// <title>Summation of primes</title>
    /// <summary>
    /// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    /// Find the sum of all the primes below two million.
    /// </summary>

    open Sequences

    type Problem010 () =

        let NumberSolve n = Sequences.Primes |> Seq.takeWhile ((>=) n) |> Seq.sum

        member this.Solve () = NumberSolve 2000000L |> string
        interface IProblem with
            member this.Solve () = this.Solve ()