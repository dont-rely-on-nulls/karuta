open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = Lexing.dummy_pos in
  let endl = step 5 startl in

  let p : (string, string) parser =
   fun state ->
    let modified_state = { remaining = state.remaining; loc = endl } in
    Ok ("matched", modified_state)
  in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  match is p state with
  | Error error -> failf "is should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check string "returns parsed value" "matched" value;

      check string "restores remaining input" "hello"
        (BatSubstring.to_string resulting_state.remaining);

      check int "restores position" startl.pos_cnum resulting_state.loc.pos_cnum;

      check int "restores line" startl.pos_lnum resulting_state.loc.pos_lnum;

      check int "restores beginning of line" startl.pos_bol
        resulting_state.loc.pos_bol

let fails_when_parser_fails () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = half_dummy }
  in

  match is p state with
  | Ok _ -> fail "is should fail when p fails"
  | Error error -> check string "propagates parser error" "p failed" error

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails" `Quick fails_when_parser_fails;
  ]
