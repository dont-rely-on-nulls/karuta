open Lib.Parser
open Lib.Location
open Alcotest

let succeeds_with_lf () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "\nabc"; loc = startl }
  in

  match line state with
  | Error _ -> fail "line should succeed with LF newline"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes LF newline" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances line number" 1 resulting_state.loc.pos_lnum;

      check int "advances position by one" 1 resulting_state.loc.pos_cnum

let succeeds_with_crlf () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "\r\nabc"; loc = startl }
  in

  match line state with
  | Error _ -> fail "line should succeed with CRLF newline"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes CRLF newline" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances line number" 1 resulting_state.loc.pos_lnum;

      check int "advances position by two" 2 resulting_state.loc.pos_cnum

let succeeds_with_text_without_newline () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match line state with
  | Error _ -> fail "line should consume text without a newline"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes all text" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by three" 3 resulting_state.loc.pos_cnum

let succeeds_with_empty_input () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = startl }
  in

  match line state with
  | Error _ -> fail "line should succeed with empty input"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "preserves empty input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position" startl.pos_cnum
        resulting_state.loc.pos_cnum;

      check int "preserves line number" startl.pos_lnum
        resulting_state.loc.pos_lnum

let tests =
  [
    test_case "succeeds with LF" `Quick succeeds_with_lf;
    test_case "succeeds with CRLF" `Quick succeeds_with_crlf;
    test_case "succeeds with text without newline" `Quick
      succeeds_with_text_without_newline;
    test_case "succeeds with empty input" `Quick succeeds_with_empty_input;
  ]
