namespace Fs

    //

    type Problem011 () =

        let NumberSolve = 0
        
        member this.Solve () = NumberSolve |> string
        interface IProblem with
            member this.Solve () = this.Solve ()

