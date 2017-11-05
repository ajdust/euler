module Tests

open Problems
open Expecto
open Newtonsoft.Json
open System.Collections.Generic

let answers =
    let path = "../../answers.json"
    let text = System.IO.File.ReadAllText(path)
    in JsonConvert.DeserializeObject<Dictionary<string,string>>(text)

[<Tests>]
let tests =
    testList "Problems 1 through 11" [
        test "Problem 1" {
            let p = Problem01 ()
            in Expect.equal (p.Solve ()) answers.["1"] "1 is correct."
        }

        test "Problem 2" {
            let p = Problem02 ()
            in Expect.equal (p.Solve ()) answers.["2"] "2 is correct."
        }

        test "Problem 3" {
            let p = Problem03 ()
            in Expect.equal (p.Solve ()) answers.["3"] "3 is correct."
        }

        test "Problem 4" {
            let p = Problem04 ()
            in Expect.equal (p.Solve ()) answers.["4"] "4 is correct."
        }
            
        test "Problem 5" {
            let p = Problem05 ()
            in Expect.equal (p.Solve ()) answers.["5"] "5 is correct."
        }
            
        test "Problem 6" {
            let p = Problem06 ()
            in Expect.equal (p.Solve ()) answers.["6"] "6 is correct."
        }
            
        test "Problem 7" {
            let p = Problem07 ()
            in Expect.equal (p.Solve ()) answers.["7"] "7 is correct."
        }
            
        test "Problem 8" {
            let p = Problem08 ()
            in Expect.equal (p.Solve ()) answers.["8"] "8 is correct."
        }
            
        test "Problem 9" {
            let p = Problem09 ()
            in Expect.equal (p.Solve ()) answers.["9"] "9 is correct."
        }
            
        test "Problem 10" {
            let p = Problem10 ()
            in Expect.equal (p.Solve ()) answers.["10"] "10 is correct."
        }

        test "Problem 11" {
            let p = Problem11 ()
            in Expect.equal (p.Solve ()) answers.["11"] "11 is correct."
        }

        test "Problem 12" {
            let p = Problem12 ()
            in Expect.equal (p.Solve ()) answers.["12"] "12 is correct."
        }
    ]

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
