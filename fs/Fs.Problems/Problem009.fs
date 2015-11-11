(*

Author: Aaron Johnson
Date:   2015-07-05

Solves Euler Problem 9.

*)
namespace Fs

    /// <title>Special Pythagorean triplet</title>
    /// <summary>
    /// A Pythagorean triplet is a set of three natural numbers, a LT b LT c, for which,
    /// a^2 + b^2 = c^2
    /// 
    /// For example, 32 + 42 = 9 + 16 = 25 = 52.
    /// 
    /// There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    /// Find the product a*b*c. 
    /// </summary>

    open Sequences

    type Problem009 () =

        let pythaAddsTo1000 =
            Seq.tryFind (fun (a, b, c) -> a + b + c = 1000) Sequences.PythagoreanTriplets

        let NumberSolve = match pythaAddsTo1000 with
                    | Some (a, b, c) -> a * b * c
                    | _ -> 0

        member this.Solve () = NumberSolve |> string
        interface IProblem with
            member this.Solve () = this.Solve ()


