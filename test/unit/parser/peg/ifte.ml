open Lib.Parser
open Alcotest

let success () =
  let test input = Ok ("parsed", input) in
  let consequent (value, state) = Ok (String.length value) in
  let alternative _ = Ok 999 in

  let actual = ifte test consequent alternative "input" in

  check (result int string) "consequent is used" (Ok 6) actual

let failure () =
  let test _ = Error "test failed" in
  let consequent _ = Ok 1 in
  let alternative input = Ok (String.length input) in

  let actual = ifte test consequent alternative "hello" in

  check (result int string) "alternative is used" (Ok 5) actual

let does_not_run_alternative_on_success () =
  let test input = Ok ("parsed", input) in
  let consequent (_value, _state) = Ok 42 in
  let alternative _ = failwith "alternative should not be called" in

  let actual = ifte test consequent alternative "input" in

  check (result int string) "alternative is not called" (Ok 42) actual

let does_not_run_consequent_on_failure () =
  let test _ = Error "test failed" in
  let consequent _ = failwith "consequent should not be called" in
  let alternative _ = Ok 42 in

  let actual = ifte test consequent alternative "input" in

  check (result int string) "consequent is not called" (Ok 42) actual

let tests =
  [
    test_case "uses consequent on success" `Quick success;
    test_case "uses alternative on failure" `Quick failure;
    test_case "does not run alternative on success" `Quick
      does_not_run_alternative_on_success;
    test_case "does not run consequent on failure" `Quick
      does_not_run_consequent_on_failure;
  ]
