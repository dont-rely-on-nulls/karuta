open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match some state with
  | Error `UnexpectedEOF -> fail "some should succeed when input remains"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes one character" "bc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by one" 1 resulting_state.loc.pos_cnum;

      check int "preserves line" startl.pos_lnum resulting_state.loc.pos_lnum;

      check int "preserves beginning of line" startl.pos_bol
        resulting_state.loc.pos_bol

let fails_when_input_is_empty () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = half_dummy }
  in

  match some state with
  | Ok _ -> fail "some should fail when input is empty"
  | Error `UnexpectedEOF -> check bool "returns UnexpectedEOF" true true

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails when input is empty" `Quick fails_when_input_is_empty;
  ]
