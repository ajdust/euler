namespace Fs

    /// <title>Largest palindrome product</title>
    /// <summary>
    /// A palindromic number reads the same both ways. The largest palindrome
    /// made from the product of two 2-digit numbers is 9009 = 91 × 99.
    /// Find the largest palindrome made from the product of two 3-digit numbers.
    /// </summary>

    open System
    open System.Linq

    type Problem004 () =

        let IsPalindrome (s : string) =
            let half = s.Length / 2
            in Seq.forall2
                (fun x y -> x = y)
                (Seq.take half s) (Seq.take half (s.Reverse()))

        let NumberSolve =
            query {
                for i in Enumerable.Range(100, 899) do
                for j in Enumerable.Range(101, 899) do
                where (IsPalindrome (Convert.ToString (i*j)))
                select (i * j)
            } |> Seq.max

        member this.Solve () = NumberSolve |> string
        interface IProblem with
            member this.Solve () = this.Solve ()
