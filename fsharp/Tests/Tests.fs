module Tests

open FsUnit
open Problems
open Xunit
open Newtonsoft.Json
open System.Collections.Generic

let answers =
    let path = "../../answers.json"
    let text = System.IO.File.ReadAllText(path)
    in JsonConvert.DeserializeObject<Dictionary<string,string>>(text)

[<Fact>]
let ``Problem 1`` () =
    let p = Problem01 ()
    in p.Solve () |> should equal answers.["1"]

[<Fact>]
let ``Problem 2`` () =
    let p = Problem02 ()
    in p.Solve () |> should equal answers.["2"]

[<Fact>]
let ``Problem 3`` () =
    let p = Problem03 ()
    in p.Solve () |> should equal answers.["3"]

[<Fact>]
let ``Problem 4`` () =
    let p = Problem04 ()
    in p.Solve () |> should equal answers.["4"]
    
[<Fact>]
let ``Problem 5`` () =
    let p = Problem05 ()
    in p.Solve () |> should equal answers.["5"]
    
[<Fact>]
let ``Problem 6`` () =
    let p = Problem06 ()
    in p.Solve () |> should equal answers.["6"]
    
[<Fact>]
let ``Problem 7`` () =
    let p = Problem07 ()
    in p.Solve () |> should equal answers.["7"]
    
[<Fact>]
let ``Problem 8`` () =
    let p = Problem08 ()
    in p.Solve () |> should equal answers.["8"]
    
[<Fact>]
let ``Problem 9`` () =
    let p = Problem09 ()
    in p.Solve () |> should equal answers.["9"]
    
[<Fact>]
let ``Problem 10`` () =
    let p = Problem10 ()
    in p.Solve () |> should equal answers.["10"]

[<Fact>]
let ``Problem 11`` () =
    let p = Problem11 ()
    in p.Solve () |> should equal answers.["11"]