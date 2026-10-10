open Lib.Parser
open Lib.Location
open Alcotest

let succeeds_with_lowercase_atom () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = startl }
  in

  match atom state with
  | Error _ -> fail "atom should accept a lowercase identifier"
  | Ok (value, resulting_state) ->
      check string "captures atom text" "hello" @@ strip_loc value;

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let succeeds_with_allowed_continuation_characters () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "aBC09_x-y"; loc = startl }
  in

  match atom state with
  | Error _ -> fail "atom should accept valid continuation characters"
  | Ok (value, resulting_state) ->
      check string "captures atom text" "aBC09_x-y" @@ strip_loc value;

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by nine" 9 resulting_state.loc.pos_cnum

let stops_at_invalid_continuation () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello.world"; loc = startl }
  in

  match atom state with
  | Error _ -> fail "atom should stop at an invalid continuation character"
  | Ok (value, resulting_state) ->
      check string "captures atom before period" "hello" @@ strip_loc value;

      check string "leaves period and following text" ".world"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let fails_when_first_character_is_uppercase () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "Hello"; loc = startl }
  in

  match atom state with
  | Ok _ -> fail "atom should require a lowercase first character"
  | Error (`ExpectedLowercase loc) ->
      check int "reports original position" startl.pos_cnum startl.pos_cnum
  | Error _ -> fail "atom should fail with ExpectedLowercase"

let fails_when_first_character_is_digit () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "123abc"; loc = startl }
  in

  match atom state with
  | Ok _ -> fail "atom should reject a digit as its first character"
  | Error (`ExpectedLowercase loc) ->
      check int "reports original position" startl.pos_cnum startl.pos_cnum
  | Error _ -> fail "atom should fail with ExpectedLowercase"

let fails_when_input_is_empty () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = half_dummy }
  in

  match atom state with
  | Ok _ -> fail "atom should fail on empty input"
  | Error `UnexpectedEOF -> ()
  | Error _ -> fail "atom should fail with UnexpectedEOF"

let tests =
  [
    test_case "succeeds with lowercase atom" `Quick succeeds_with_lowercase_atom;
    test_case "succeeds with allowed continuation characters" `Quick
      succeeds_with_allowed_continuation_characters;
    test_case "stops at invalid continuation" `Quick
      stops_at_invalid_continuation;
    test_case "fails when first character is uppercase" `Quick
      fails_when_first_character_is_uppercase;
    test_case "fails when first character is digit" `Quick
      fails_when_first_character_is_digit;
    test_case "fails on empty input" `Quick fails_when_input_is_empty;
  ]
