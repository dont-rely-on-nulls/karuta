open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_coordinate in
  let endl = step 5 startl in

  let p : (string, string) parser =
   fun state ->
    let modified_state =
      { remaining = state.remaining; loc = dummy_coord_to_point endl }
    in
    Ok ("matched", modified_state)
  in

  let state : parser_state =
    {
      remaining = BatSubstring.of_string "hello";
      loc = dummy_coord_to_point startl;
    }
  in

  match is p state with
  | Error error -> failf "is should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check string "returns parsed value" "matched" value;

      check string "restores remaining input" "hello"
        (BatSubstring.to_string resulting_state.remaining);

      check int "restores position" startl.offset
        resulting_state.loc.coordinate.offset;

      check int "restores line" startl.line resulting_state.loc.coordinate.line;

      check int "restores beginning of line" startl.line_offset
        resulting_state.loc.coordinate.line_offset

let fails_when_parser_fails () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = zero_point }
  in

  match is p state with
  | Ok _ -> fail "is should fail when p fails"
  | Error error -> check string "propagates parser error" "p failed" error

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "fails" `Quick fails_when_parser_fails;
  ]
