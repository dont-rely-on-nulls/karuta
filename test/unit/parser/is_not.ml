open Lib.Parser
open Lib.Location
open Alcotest

let fails_when_parser_succeeds () =
  let p : (string, string) parser = fun state -> Ok ("matched", state) in

  let handler value _loc = "unexpectedly matched: " ^ value in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = half_dummy }
  in

  let actual = is_not p handler state in

  match actual with
  | Ok _ -> fail "is_not should fail when p succeeds"
  | Error error ->
      check string "returns handler error" "unexpectedly matched: matched" error

let succeeds_when_parser_fails () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let handler _value _loc = "handler should not be called" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = half_dummy }
  in

  match is_not p handler state with
  | Error error -> failf "is_not should succeed, but returned: %s" error
  | Ok ((), resulting_state) ->
      check string "remaining input is unchanged" "hello"
        (BatSubstring.to_string resulting_state.remaining);

      check int "line is unchanged" state.loc.pos_lnum
        resulting_state.loc.pos_lnum;

      check int "position is unchanged" state.loc.pos_cnum
        resulting_state.loc.pos_cnum;

      check int "beginning of line is unchanged" state.loc.pos_bol
        resulting_state.loc.pos_bol

let handler_receives_correct_location () =
  let startl = Lexing.dummy_pos in
  let endl = step 5 startl in

  let p : (string, string) parser =
   fun state ->
    let new_state = { state with loc = endl } in
    Ok ("matched", new_state)
  in

  let handler value loc = (value, loc) in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  let actual = is_not p handler state in

  match actual with
  | Ok _ -> fail "is_not should fail when p succeeds"
  | Error (value, loc) ->
      check string "handler receives parser result" "matched" value;

      check int "handler receives original start position" startl.pos_cnum
        loc.startl.pos_cnum;

      check int "handler receives parser end position" endl.pos_cnum
        loc.endl.pos_cnum

let does_not_call_handler_when_parser_fails () =
  let p : (string, string) parser = fun _state -> Error "p failed" in

  let handler _value _loc = failwith "handler should not be called" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = half_dummy }
  in

  match is_not p handler state with
  | Error _ -> fail "is_not should succeed when p fails"
  | Ok ((), _) -> ()

let tests =
  [
    test_case "fails when parser succeeds" `Quick fails_when_parser_succeeds;
    test_case "succeeds when parser fails" `Quick succeeds_when_parser_fails;
    test_case "handler receives correct location" `Quick
      handler_receives_correct_location;
    test_case "does not call handler when parser fails" `Quick
      does_not_call_handler_when_parser_fails;
  ]
