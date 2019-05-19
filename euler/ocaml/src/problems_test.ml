open OUnit2 ;;
open Yojson ;;

let answer n =
    let json = Yojson.Basic.from_file "../../answers.json" in
    let open Yojson.Basic.Util in
    json |> member (string_of_int n) |> to_string

let ne_msg expected actual =
    "Expected " ^ expected ^ " but have " ^ actual

let test_problem01 context =
    let expected = answer 1 in
    let actual = Problems.problem01 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem02 context =
    let expected = answer 2 in
    let actual = Problems.problem02 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem03 context =
    let expected = answer 3 in
    let actual = Problems.problem03 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem04 context =
    let expected = answer 4 in
    let actual = Problems.problem04 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem05 context =
    let expected = answer 5 in
    let actual = Problems.problem05 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem06 context =
    let expected = answer 6 in
    let actual = Problems.problem06 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem07 context =
    let expected = answer 7 in
    let actual = Problems.problem07 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem08 context =
    let expected = answer 8 in
    let actual = Problems.problem08 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem09 context =
    let expected = answer 9 in
    let actual = Problems.problem09 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem10 context =
    let expected = answer 10 in
    let actual = Problems.problem10 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem11 context =
    let expected = answer 11 in
    let actual = Problems.problem11 () in
    assert_equal expected actual ~msg:(ne_msg expected actual)

let test_problem12 context =
    let expected = answer 12 in
    let actual = Problems.problem12 () in
    begin
        Printf.printf "Test 12 passed: got %s" actual;
        assert_equal expected actual ~msg:(ne_msg expected actual)
    end

let problem_tests = "test the problems" >:::
[
  "test problem 1" >:: test_problem01 ;
  "test problem 2" >:: test_problem02 ;
  "test problem 3" >:: test_problem03 ;
  "test problem 4" >:: test_problem04 ;
  "test problem 5" >:: test_problem05 ;
  "test problem 6" >:: test_problem06 ;
  "test problem 7" >:: test_problem07 ;
  "test problem 8" >:: test_problem08 ;
  "test problem 9" >:: test_problem09 ;
  "test problem 10" >:: test_problem10 ;
  "test problem 11" >:: test_problem11 ;
  "test problem 12" >:: test_problem12 ;
]

let _ = run_test_tt_main problem_tests
