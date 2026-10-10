open Lib.Parser
open Lib.Location
module FT = Lib.FT
open Alcotest

let stepped_seven = dummy_coord_to_point @@ step 7 zero_coordinate

let parse_func input =
  let state : parser_state =
    { remaining = BatSubstring.of_string input; loc = zero_point }
  in
  match func state with
  | Ok (value, _) -> value
  | Error _ -> failf "could not parse initial functor from %S" input

let succeeds_with_question_mark () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    { remaining = BatSubstring.of_string "? rest"; loc = stepped_seven }
  in

  match query first_element state with
  | Error _ -> fail "query should succeed when it encounters a question mark"
  | Ok (value, resulting_state) ->
      check int "returns one functor" 1 (FT.size value.content);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "consume the question mark" 8
        resulting_state.loc.coordinate.offset

let succeeds_with_multiple_functors () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    {
      remaining = BatSubstring.of_string ", second(), third()?";
      loc = stepped_seven;
    }
  in

  match query first_element state with
  | Error _ -> fail "query should parse multiple comma-separated functors"
  | Ok (value, resulting_state) ->
      check int "returns three functors" 3 (FT.size value.content);

      check string "does not leave the question mark" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by twenty-seven" 27
        resulting_state.loc.coordinate.offset

let succeeds_with_whitespace_between_functors () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    {
      remaining = BatSubstring.of_string "  ,  second()  ?";
      loc = stepped_seven;
    }
  in

  match query first_element state with
  | Error _ -> fail "query should accept whitespace between functors"
  | Ok (value, resulting_state) ->
      check int "returns two functors" 2 (FT.size value.content);

      check string "does not leave the question mark" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int
        "advances position past whitespace, second functor, and question mark"
        23 resulting_state.loc.coordinate.offset

let succeeds_with_line_comment_between_functors () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    {
      remaining = BatSubstring.of_string "% comment\n, second() ?";
      loc = stepped_seven;
    }
  in

  match query first_element state with
  | Error _ -> fail "query should accept a comment between functors"
  | Ok (value, resulting_state) ->
      check int "returns two functors" 2 (FT.size value.content);

      check string "does not leave the question mark" ""
        (BatSubstring.to_string resulting_state.remaining)

let succeeds_with_immediate_termination () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    { remaining = BatSubstring.of_string "?"; loc = stepped_seven }
  in

  match query first_element state with
  | Error _ -> fail "query should allow immediate termination"
  | Ok (value, resulting_state) ->
      check int "returns the initial functor only" 1 (FT.size value.content);

      check string "does not leave the question mark" ""
        (BatSubstring.to_string resulting_state.remaining)

let fails_when_comma_is_missing () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    { remaining = BatSubstring.of_string " second()?"; loc = stepped_seven }
  in

  match query first_element state with
  | Ok _ -> fail "query should require a comma before the next functor"
  | Error _ -> ()

let fails_when_functor_is_missing_after_comma () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    { remaining = BatSubstring.of_string ", ?"; loc = stepped_seven }
  in

  match query first_element state with
  | Ok _ -> fail "query should require a functor after the comma"
  | Error _ -> ()

let fails_when_input_ends_after_comma () =
  let first_element = parse_func "first()" in
  let state : parser_state =
    { remaining = BatSubstring.of_string ","; loc = stepped_seven }
  in

  match query first_element state with
  | Ok _ -> fail "query should fail when input ends after the comma"
  | Error _ -> ()

let tests =
  [
    test_case "succeeds with question mark" `Quick succeeds_with_question_mark;
    test_case "succeeds with multiple functors" `Quick
      succeeds_with_multiple_functors;
    test_case "succeeds with whitespace between functors" `Quick
      succeeds_with_whitespace_between_functors;
    test_case "succeeds with line comment between functors" `Quick
      succeeds_with_line_comment_between_functors;
    test_case "succeeds with immediate termination" `Quick
      succeeds_with_immediate_termination;
    test_case "fails when comma is missing" `Quick fails_when_comma_is_missing;
    test_case "fails when functor is missing after comma" `Quick
      fails_when_functor_is_missing_after_comma;
    test_case "fails when input ends after comma" `Quick
      fails_when_input_ends_after_comma;
  ]
