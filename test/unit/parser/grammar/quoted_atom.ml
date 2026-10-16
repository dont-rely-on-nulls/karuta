open Lib.Parser
open Lib.Location
open Alcotest

let succeeds () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "'hello' rest"; loc = startl }
  in

  match quoted_atom state with
  | Error _ -> fail "quoted_atom should parse a quoted atom"
  | Ok (value, resulting_state) ->
      check string "captures atom text" "hello" @@ strip_loc value;

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by seven" 7
        resulting_state.loc.coordinate.offset

let succeeds_with_empty_atom () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "''"; loc = startl }
  in

  match quoted_atom state with
  | Error _ -> fail "quoted_atom should accept an empty quoted atom"
  | Ok (value, resulting_state) ->
      check string "captures empty atom" "" @@ strip_loc value;

      check string "consumes both quotes" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by two" 2
        resulting_state.loc.coordinate.offset

let succeeds_with_special_characters () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "'hello world-123_!'"; loc = startl }
  in

  match quoted_atom state with
  | Error _ -> fail "quoted_atom should accept special characters"
  | Ok (value, resulting_state) ->
      check string "captures atom text" "hello world-123_!" @@ strip_loc value;

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by nineteen" 19
        resulting_state.loc.coordinate.offset

let fails_without_opening_quote () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello'"; loc = startl }
  in

  match quoted_atom state with
  | Ok _ -> fail "quoted_atom should require an opening quote"
  | Error (`WrongPrefix (loc, expected)) ->
      check string "expects opening quote" "'" expected;
      check int "reports original position" startl.coordinate.offset
        startl.coordinate.offset
  | Error _ -> fail "quoted_atom should fail with WrongPrefix"

let fails_without_closing_quote () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "'hello"; loc = startl }
  in

  match quoted_atom state with
  | Ok _ -> fail "quoted_atom should require a closing quote"
  | Error (`WrongPrefix (loc, expected)) ->
      check string "expects closing quote" "'" expected;
      check int "reports original position" 0 startl.coordinate.offset
  | Error _ -> fail "quoted_atom should fail with WrongPrefix"

let fails_when_newline_occurs_before_closing_quote () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "'hello\nworld'"; loc = startl }
  in

  match quoted_atom state with
  | Ok _ -> fail "quoted_atom should reject a newline inside the atom"
  | Error (`WrongPrefix (loc, expected)) ->
      check string "expects closing quote" "'" expected;
      check int "reports original position" 0 startl.coordinate.offset
  | Error _ -> fail "quoted_atom should fail with WrongPrefix"

let fails_on_empty_input () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = zero_point }
  in

  match quoted_atom state with
  | Ok _ -> fail "quoted_atom should fail on empty input"
  | Error (`WrongPrefix (_, expected)) ->
      check string "expects opening quote" "'" expected
  | Error _ -> fail "quoted_atom should fail with WrongPrefix"

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "succeeds with empty atom" `Quick succeeds_with_empty_atom;
    test_case "succeeds with special characters" `Quick
      succeeds_with_special_characters;
    test_case "fails without opening quote" `Quick fails_without_opening_quote;
    test_case "fails without closing quote" `Quick fails_without_closing_quote;
    test_case "fails when newline occurs before closing quote" `Quick
      fails_when_newline_occurs_before_closing_quote;
    test_case "fails on empty input" `Quick fails_on_empty_input;
  ]
