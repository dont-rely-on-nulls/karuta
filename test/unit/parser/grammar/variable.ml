open Lib.Parser
open Lib.Location
open Alcotest

let succeeds_with_uppercase_variable () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "Hello"; loc = startl }
  in

  match variable state with
  | Error _ -> fail "variable should accept an uppercase starting character"
  | Ok (value, resulting_state) ->
      check string "captures variable text" "Hello" @@ strip_loc value;

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5
        resulting_state.loc.coordinate.offset

let succeeds_with_underscore_prefix () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "_hello"; loc = startl }
  in

  match variable state with
  | Error _ -> fail "variable should accept an underscore prefix"
  | Ok (value, resulting_state) ->
      check string "captures variable text" "_hello" @@ strip_loc value;

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by six" 6
        resulting_state.loc.coordinate.offset

let succeeds_with_allowed_continuation_characters () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "Aabc09_X"; loc = startl }
  in

  match variable state with
  | Error _ -> fail "variable should accept valid continuation characters"
  | Ok (value, resulting_state) ->
      check string "captures variable text" "Aabc09_X" @@ strip_loc value;

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by eight" 8
        resulting_state.loc.coordinate.offset

let stops_at_invalid_continuation () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "Hello-world"; loc = startl }
  in

  match variable state with
  | Error _ -> fail "variable should stop at a hyphen"
  | Ok (value, resulting_state) ->
      check string "captures variable before hyphen" "Hello" @@ strip_loc value;

      check string "leaves hyphen and following text" "-world"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5
        resulting_state.loc.coordinate.offset

let fails_when_first_character_is_lowercase () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  match variable state with
  | Ok _ -> fail "variable should reject a lowercase starting character"
  | Error (`ExpectedUppercaseOrUnderscore loc) ->
      check int "reports original position" startl.coordinate.offset
        startl.coordinate.offset
  | Error _ -> fail "atom should fail with ExpectedUppercaseOrUnderscore"

let fails_when_first_character_is_digit () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "123abc"; loc = startl }
  in

  match variable state with
  | Ok _ -> fail "variable should reject a digit as its first character"
  | Error (`ExpectedUppercaseOrUnderscore loc) ->
      check int "reports original position" startl.coordinate.offset
        startl.coordinate.offset
  | Error _ -> fail "atom should fail with ExpectedUppercaseOrUnderscore"

let fails_when_input_is_empty () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = zero_point }
  in

  match variable state with
  | Ok _ -> fail "variable should fail on empty input"
  | Error `UnexpectedEOF -> ()
  | Error _ -> fail "atom should fail with UnexpectedEOF"

let tests =
  [
    test_case "succeeds with uppercase variable" `Quick
      succeeds_with_uppercase_variable;
    test_case "succeeds with underscore prefix" `Quick
      succeeds_with_underscore_prefix;
    test_case "succeeds with allowed continuation characters" `Quick
      succeeds_with_allowed_continuation_characters;
    test_case "stops at invalid continuation" `Quick
      stops_at_invalid_continuation;
    test_case "fails when first character is lowercase" `Quick
      fails_when_first_character_is_lowercase;
    test_case "fails when first character is digit" `Quick
      fails_when_first_character_is_digit;
    test_case "fails on empty input" `Quick fails_when_input_is_empty;
  ]
