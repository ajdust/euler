namespace Fs

    /// <title>Largest prime factor</title>
    /// <summary>
    /// The prime factors of 13195 are 5, 7, 13 and 29. 
    /// What is the largest prime factor of the number 600851475143 ?
    /// </summary>

    open Sequences

    type Problem003 () =
        
        let NumberSolve n = PrimeFactors (n : int64) |> Seq.max

        member this.Solve () = NumberSolve 600851475143L |> string
        interface IProblem with
            member this.Solve () = this.Solve ()