open Lib.Parser
open Lib.Location
open Alcotest

let succeeds_with_spaces () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "   abc"; loc = startl }
  in

  match horizontal_whitespace state with
  | Error (`ExpectedHorizontalWhitespace _) ->
      fail "horizontal_whitespace should accept spaces"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes all leading spaces" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by three" 3
        resulting_state.loc.coordinate.offset

let succeeds_with_tabs () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "\t\tabc"; loc = startl }
  in

  match horizontal_whitespace state with
  | Error (`ExpectedHorizontalWhitespace _) ->
      fail "horizontal_whitespace should accept tabs"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes all leading tabs" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by two" 2
        resulting_state.loc.coordinate.offset

let succeeds_with_mixed_whitespace () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string " \t \t abc"; loc = startl }
  in

  match horizontal_whitespace state with
  | Error (`ExpectedHorizontalWhitespace _) ->
      fail "horizontal_whitespace should accept mixed whitespace"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "consumes all leading horizontal whitespace" "abc"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5
        resulting_state.loc.coordinate.offset

let stops_at_non_whitespace () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string " \t abc \t"; loc = startl }
  in

  match horizontal_whitespace state with
  | Error (`ExpectedHorizontalWhitespace _) ->
      fail "horizontal_whitespace should accept leading whitespace"
  | Ok (value, resulting_state) ->
      check unit "returns unit" () value;

      check string "leaves non-whitespace and following input" "abc \t"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by three" 3
        resulting_state.loc.coordinate.offset

let fails_without_horizontal_whitespace () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match horizontal_whitespace state with
  | Ok _ -> fail "horizontal_whitespace should fail without whitespace"
  | Error (`ExpectedHorizontalWhitespace loc) ->
      check int "reports the original position" startl.coordinate.offset
        startl.coordinate.offset

let fails_on_empty_input () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = startl }
  in

  match horizontal_whitespace state with
  | Ok _ -> fail "horizontal_whitespace should fail on empty input"
  | Error (`ExpectedHorizontalWhitespace loc) ->
      check int "reports the original position" startl.coordinate.offset
        startl.coordinate.offset

let tests =
  [
    test_case "succeeds with spaces" `Quick succeeds_with_spaces;
    test_case "succeeds with tabs" `Quick succeeds_with_tabs;
    test_case "succeeds with mixed whitespace" `Quick
      succeeds_with_mixed_whitespace;
    test_case "stops at non-whitespace" `Quick stops_at_non_whitespace;
    test_case "fails without horizontal whitespace" `Quick
      fails_without_horizontal_whitespace;
    test_case "fails on empty input" `Quick fails_on_empty_input;
  ]
