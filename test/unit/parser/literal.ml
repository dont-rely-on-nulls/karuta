open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello world"; loc = startl }
  in

  match literal "hello" state with
  | Error (`WrongPrefix _) -> fail "literal should match the input prefix"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes the matched prefix" " world"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let fails_when_prefix_does_not_match () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "world"; loc = startl }
  in

  match literal "hello" state with
  | Ok _ -> fail "literal should fail when the prefix does not match"
  | Error (`WrongPrefix (loc, expected)) ->
      check string "reports expected literal" "hello" expected;
      check int "reports original position" startl.pos_cnum startl.pos_cnum

let fails_when_input_is_too_short () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hel"; loc = startl }
  in

  match literal "hello" state with
  | Ok _ -> fail "literal should fail when input is shorter than the literal"
  | Error (`WrongPrefix (loc, expected)) ->
      check string "reports expected literal" "hello" expected;
      check int "reports original position" startl.pos_cnum startl.pos_cnum

let succeeds_with_empty_literal () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  match literal "" state with
  | Error (`WrongPrefix _) -> fail "an empty literal should match any input"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "preserves remaining input" "hello"
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position" startl.pos_cnum
        resulting_state.loc.pos_cnum

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails when prefix does not match" `Quick
      fails_when_prefix_does_not_match;
    test_case "fails when input is too short" `Quick
      fails_when_input_is_too_short;
    test_case "succeeds with empty literal" `Quick succeeds_with_empty_literal;
  ]
