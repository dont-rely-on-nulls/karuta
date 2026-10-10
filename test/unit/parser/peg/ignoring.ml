open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_half in
  let endl = step 5 startl in

  let p : (string, string) parser =
   fun state ->
    let modified_state =
      { remaining = BatSubstring.of_string ""; loc = endl }
    in
    Ok ("ignored", modified_state)
  in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  let captured = "captured" in

  match ignoring p (captured, state) with
  | Error error -> failf "ignoring should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check string "returns captured output" captured value;

      check string "preserves remaining input from parser" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position from parser" endl.pos_cnum
        resulting_state.loc.pos_cnum;

      check int "preserves line from parser" endl.pos_lnum
        resulting_state.loc.pos_lnum;

      check int "preserves beginning of line from parser" endl.pos_bol
        resulting_state.loc.pos_bol

let fails_when_parser_fails () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = zero_half }
  in

  match ignoring p ("captured", state) with
  | Ok _ -> fail "ignoring should fail when the parser fails"
  | Error error -> check string "propagates parser error" "p failed" error

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails when parser fails" `Quick fails_when_parser_fails;
  ]
