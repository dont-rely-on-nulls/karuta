open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_half in
  let endl = step 5 startl in

  let p : (string, string) parser =
   fun state ->
    let modified_state = { remaining = BatSubstring.empty (); loc = endl } in
    Ok ("matched", modified_state)
  in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  match maybe p state with
  | Error error -> failf "maybe should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (option string) "returns Some parsed value" (Some "matched") value;

      check string "preserves remaining input from parser" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position from parser" endl.pos_cnum
        resulting_state.loc.pos_cnum;

      check int "preserves line from parser" endl.pos_lnum
        resulting_state.loc.pos_lnum;

      check int "preserves beginning of line from parser" endl.pos_bol
        resulting_state.loc.pos_bol

let fails_and_backtracks () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = zero_half }
  in

  match maybe p state with
  | Error error ->
      failf "maybe should succeed with None, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (option string) "returns None when parser fails" None value;

      check string "restores remaining input" "hello"
        (BatSubstring.to_string resulting_state.remaining);

      check int "restores position" state.loc.pos_cnum
        resulting_state.loc.pos_cnum;

      check int "restores line" state.loc.pos_lnum resulting_state.loc.pos_lnum;

      check int "restores beginning of line" state.loc.pos_bol
        resulting_state.loc.pos_bol

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails and backtracks" `Quick fails_and_backtracks;
  ]
