open Lib.Parser
open Lib.Location
open Alcotest

let succeeds_with_horizontal_whitespace () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "   abc"; loc = startl }
  in

  match whitespace state with
  | Error _ -> fail "whitespace should accept horizontal whitespace"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes leading spaces" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by three" 3 resulting_state.loc.pos_cnum

let succeeds_with_tabs () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "\t\tabc"; loc = startl }
  in

  match whitespace state with
  | Error _ -> fail "whitespace should accept tabs"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes leading tabs" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by two" 2 resulting_state.loc.pos_cnum

let succeeds_with_newline () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "\nabc"; loc = startl }
  in

  match whitespace state with
  | Error _ -> fail "whitespace should accept a newline"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes newline" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances line number" 1 resulting_state.loc.pos_lnum

let succeeds_with_multiple_lines () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string " \n\t\n  abc"; loc = startl }
  in

  match whitespace state with
  | Error _ -> fail "whitespace should accept multiple lines"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes whitespace across lines" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances line number twice" 2 resulting_state.loc.pos_lnum

let succeeds_with_empty_input () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = startl }
  in

  match whitespace state with
  | Error _ -> fail "whitespace should succeed on empty input"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "preserves empty input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position" startl.pos_cnum
        resulting_state.loc.pos_cnum

let succeeds_without_consuming_non_whitespace () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match whitespace state with
  | Error _ -> fail "whitespace should succeed without whitespace"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "preserves non-whitespace input" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position" startl.pos_cnum
        resulting_state.loc.pos_cnum

let tests =
  [
    test_case "succeeds with horizontal whitespace" `Quick
      succeeds_with_horizontal_whitespace;
    test_case "succeeds with tabs" `Quick succeeds_with_tabs;
    test_case "succeeds with newline" `Quick succeeds_with_newline;
    test_case "succeeds with multiple lines" `Quick succeeds_with_multiple_lines;
    test_case "succeeds with empty input" `Quick succeeds_with_empty_input;
    test_case "succeeds without consuming non-whitespace" `Quick
      succeeds_without_consuming_non_whitespace;
  ]
