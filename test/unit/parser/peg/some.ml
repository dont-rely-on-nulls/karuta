open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match some state with
  | Error `UnexpectedEOF -> fail "some should succeed when input remains"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes one character" "bc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by one" 1
        resulting_state.loc.coordinate.offset;

      check int "preserves line" startl.coordinate.line
        resulting_state.loc.coordinate.line;

      check int "preserves beginning of line" startl.coordinate.line_offset
        resulting_state.loc.coordinate.line_offset

let fails_when_input_is_empty () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = zero_point }
  in

  match some state with
  | Ok _ -> fail "some should fail when input is empty"
  | Error `UnexpectedEOF -> check bool "returns UnexpectedEOF" true true

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails when input is empty" `Quick fails_when_input_is_empty;
  ]
