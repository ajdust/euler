module eulerfsharp.Problem010
(*

Author: Aaron Johnson
Date:   2015-07-19

Solves Euler Problem 10.

*)

/// <title>Summation of primes</title>
/// <summary>
/// The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
/// Find the sum of all the primes below two million.
/// </summary>

open Sequences

let Solve n = Sequences.bigPrimes |> Seq.takeWhile ((>=) n) |> Seq.sum