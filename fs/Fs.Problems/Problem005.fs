module eulerfsharp.Problem005

/// <title>Smallest multiple</title>
/// <summary>
/// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
/// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
/// </summary>
/// <remarks>
/// This problem builds off of problem 3, Largest prime factor, which provided a method to list the 
/// prime factors of a number.
/// </remarks>

open Sequences
open System.Collections.Generic

let toMap dictionary = 
    (dictionary :> seq<_>)
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let CountCommonFactors n =
    Sequences.PrimeFactors n
    |> Seq.countBy (fun x -> x)
    |> dict

let TakeMaxCount (factorCount1:IDictionary<int64,int>) (factorCount2:IDictionary<int64,int>) =
    let factors = Set.ofSeq (Seq.append factorCount1.Keys factorCount2.Keys)
    in Set.map (fun key ->
        let value = 
            if factorCount1.ContainsKey(key) && factorCount2.ContainsKey(key) then List.max [factorCount1.[key]; factorCount2.[key]]
            elif factorCount1.ContainsKey(key) then factorCount1.[key]
            elif factorCount2.ContainsKey(key) then factorCount2.[key]
            else 0
        in key, value) factors
    |> dict

let CollectCommonFactors nums =
    let commonFactorCounts = Seq.map CountCommonFactors nums
    in Seq.fold
        (fun accFactorCount factorCount -> TakeMaxCount accFactorCount factorCount)
        (Seq.head commonFactorCounts)
        commonFactorCounts

let Solve n =
    let factors = CollectCommonFactors [1L .. n] |> toMap
    in Map.fold (fun acc key value -> acc * (pown key value)) 1L factors

