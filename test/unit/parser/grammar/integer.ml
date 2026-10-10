open Lib.Parser
open Lib.Location
open Alcotest

let succeeds_with_positive_integer () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "12345 rest"; loc = startl }
  in

  match integer state with
  | Error _ -> fail "integer should parse a positive integer"
  | Ok (value, resulting_state) ->
      check int "captures integer value" 12345 @@ strip_loc value;

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5
        resulting_state.loc.coordinate.offset

let succeeds_with_negative_integer () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "-123 rest"; loc = startl }
  in

  match integer state with
  | Error _ -> fail "integer should parse a negative integer"
  | Ok (value, resulting_state) ->
      check int "captures negative integer value" (-123) @@ strip_loc value;

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by four" 4
        resulting_state.loc.coordinate.offset

let fails_when_minus_has_no_digits () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "-"; loc = startl }
  in

  match integer state with
  | Ok _ -> fail "integer should fail when minus is not followed by digits"
  | Error `UnexpectedEOF -> ()
  | Error (`NotADigit _) ->
      fail "integer should return UnexpectedEOF when no input follows minus"
  | Error _ -> fail "integer returned an unexpected error"

let fails_when_first_character_is_not_a_digit () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match integer state with
  | Ok _ -> fail "integer should reject non-digit input"
  | Error (`NotADigit loc) ->
      check int "reports original position" startl.coordinate.offset
        startl.coordinate.offset
  | Error `UnexpectedEOF -> fail "input is not empty"
  | Error _ -> fail "integer returned an unexpected error"

let fails_on_empty_input () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = zero_point }
  in

  match integer state with
  | Ok _ -> fail "integer should fail on empty input"
  | Error `UnexpectedEOF -> ()
  | Error (`NotADigit _) ->
      fail "integer should return UnexpectedEOF on empty input"
  | Error _ -> fail "integer returned an unexpected error"

let succeeds_with_leading_zeros () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "000123 rest"; loc = startl }
  in

  match integer state with
  | Error _ -> fail "integer should accept leading zeros"
  | Ok (value, resulting_state) ->
      check int "parses integer value" 123 @@ strip_loc value;

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by six" 6
        resulting_state.loc.coordinate.offset

let tests =
  [
    test_case "succeeds with positive integer" `Quick
      succeeds_with_positive_integer;
    test_case "succeeds with negative integer" `Quick
      succeeds_with_negative_integer;
    test_case "succeeds with leading zeros" `Quick succeeds_with_leading_zeros;
    test_case "fails when minus has no digits" `Quick
      fails_when_minus_has_no_digits;
    test_case "fails when first character is not a digit" `Quick
      fails_when_first_character_is_not_a_digit;
    test_case "fails on empty input" `Quick fails_on_empty_input;
  ]
