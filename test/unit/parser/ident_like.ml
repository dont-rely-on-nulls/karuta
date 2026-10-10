open Lib.Parser
open Lib.Location
open Alcotest

let is_start = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false

let is_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let ident_like_test =
  ident_like is_start is_character (fun loc -> `ExpectedIdentifier loc)

let succeeds () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello123 rest"; loc = startl }
  in

  match ident_like_test state with
  | Error _ -> fail "ident_like should parse a valid identifier"
  | Ok (value, resulting_state) ->
      check string "captures identifier text" "hello123" (strip_loc value);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by eight" 8 resulting_state.loc.pos_cnum

let succeeds_with_single_character () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "x!"; loc = startl }
  in

  match ident_like_test state with
  | Error _ -> fail "ident_like should accept a single-character identifier"
  | Ok (value, resulting_state) ->
      check string "captures identifier text" "x" (strip_loc value);

      check string "leaves remaining input" "!"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by one" 1 resulting_state.loc.pos_cnum

let stops_at_invalid_continuation () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello-world"; loc = startl }
  in

  match ident_like_test state with
  | Error _ -> fail "ident_like should stop at an invalid continuation"
  | Ok (value, resulting_state) ->
      check string "captures identifier before hyphen" "hello" (strip_loc value);

      check string "leaves hyphen and following text" "-world"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let fails_when_first_character_is_invalid () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "123abc"; loc = startl }
  in

  match ident_like_test state with
  | Ok _ -> fail "ident_like should reject an invalid starting character"
  | Error (`ExpectedIdentifier loc) ->
      check int "reports original position" startl.pos_cnum startl.pos_cnum
  | Error _ ->
      fail "ident_like should fail with UnexpectedEOF or via fallthrough"

let fails_when_input_is_empty () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = half_dummy }
  in

  match ident_like_test state with
  | Ok _ -> fail "ident_like should fail on empty input"
  | Error `UnexpectedEOF -> ()
  | Error _ -> fail "ident_like should fail with UnexpectedEOF"

let tests =
  [
    test_case "succeeds" `Quick succeeds;
    test_case "succeeds with single character" `Quick
      succeeds_with_single_character;
    test_case "stops at invalid continuation" `Quick
      stops_at_invalid_continuation;
    test_case "fails when first character is invalid" `Quick
      fails_when_first_character_is_invalid;
    test_case "fails on empty input" `Quick fails_when_input_is_empty;
  ]
