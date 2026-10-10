open Lib.Parser
open Lib.Location
module FT = Lib.FT
open Alcotest

let integer_list =
  list_of ~start_delim:(literal "[") ~separator:(literal ",")
    ~end_delim:(literal "]") integer

let succeeds_with_empty_list () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[] rest"; loc = startl }
  in

  match integer_list state with
  | Error _ -> fail "list_of should accept an empty list"
  | Ok (value, resulting_state) ->
      check (list int) "returns an empty list" []
        (List.map strip_loc (FT.to_list value));

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by two" 2 resulting_state.loc.pos_cnum

let succeeds_with_one_item () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[123] rest"; loc = startl }
  in

  match integer_list state with
  | Error _ -> fail "list_of should accept a single item"
  | Ok (value, resulting_state) ->
      check (list int) "returns one item" [ 123 ]
        (List.map strip_loc (FT.to_list value));

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let succeeds_with_multiple_items () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[1,22,333] rest"; loc = startl }
  in

  match integer_list state with
  | Error _ -> fail "list_of should accept multiple items"
  | Ok (value, resulting_state) ->
      check (list int) "returns items in order" [ 1; 22; 333 ]
        (List.map strip_loc (FT.to_list value));

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by ten" 10 resulting_state.loc.pos_cnum

let fails_without_start_delimiter () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "1,2]"; loc = startl }
  in

  match integer_list state with
  | Ok _ -> fail "list_of should require the start delimiter"
  | Error _ -> ()

let fails_when_item_is_missing () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[1,]"; loc = startl }
  in

  match integer_list state with
  | Ok _ -> fail "list_of should reject a missing item after separator"
  | Error _ -> ()

let fails_without_end_delimiter () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[1,2"; loc = startl }
  in

  match integer_list state with
  | Ok _ -> fail "list_of should require the end delimiter"
  | Error _ -> ()

let fails_on_empty_input () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = half_dummy }
  in

  match integer_list state with
  | Ok _ -> fail "list_of should fail when the start delimiter is missing"
  | Error _ -> ()

let tests =
  [
    test_case "succeeds with empty list" `Quick succeeds_with_empty_list;
    test_case "succeeds with one item" `Quick succeeds_with_one_item;
    test_case "succeeds with multiple items" `Quick succeeds_with_multiple_items;
    test_case "fails without start delimiter" `Quick
      fails_without_start_delimiter;
    test_case "fails when item is missing" `Quick fails_when_item_is_missing;
    test_case "fails without end delimiter" `Quick fails_without_end_delimiter;
    test_case "fails on empty input" `Quick fails_on_empty_input;
  ]
