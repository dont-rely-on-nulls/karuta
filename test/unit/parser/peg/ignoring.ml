open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_coordinate in
  let endl = step 5 startl in

  let p : (string, string) parser =
   fun state ->
    let modified_state =
      { remaining = BatSubstring.of_string ""; loc = dummy_coord_to_point endl }
    in
    Ok ("ignored", modified_state)
  in

  let state : parser_state =
    {
      remaining = BatSubstring.of_string "hello";
      loc = dummy_coord_to_point startl;
    }
  in

  let captured = "captured" in

  match ignoring p (captured, state) with
  | Error error -> failf "ignoring should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check string "returns captured output" captured value;

      check string "preserves remaining input from parser" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position from parser" endl.offset
        resulting_state.loc.coordinate.offset;

      check int "preserves line from parser" endl.line
        resulting_state.loc.coordinate.line;

      check int "preserves beginning of line from parser" endl.line_offset
        resulting_state.loc.coordinate.line_offset

let fails_when_parser_fails () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = zero_point }
  in

  match ignoring p ("captured", state) with
  | Ok _ -> fail "ignoring should fail when the parser fails"
  | Error error -> check string "propagates parser error" "p failed" error

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails when parser fails" `Quick fails_when_parser_fails;
  ]
