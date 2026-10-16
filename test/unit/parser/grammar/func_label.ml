open Lib.Parser
open Lib.Location
module FT = Lib.FT
open Alcotest

let check_qualifiers msg expected qualifiers =
  qualifiers |> FT.to_list |> List.map strip_loc
  |> check (list string) msg expected

let succeeds_with_atom_label () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello rest"; loc = startl }
  in

  match func_label state with
  | Error _ -> fail "func_label should accept an atom label"
  | Ok (value, resulting_state) ->
      let qualifiers, label_name = strip_loc value in

      check_qualifiers "returns no qualifiers" [] qualifiers;

      check string "captures label name" "hello" (strip_loc label_name);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5
        resulting_state.loc.coordinate.offset

let succeeds_with_quoted_label () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "'hello world' rest"; loc = startl }
  in

  match func_label state with
  | Error _ -> fail "func_label should accept a quoted label"
  | Ok (value, resulting_state) ->
      let qualifiers, label_name = strip_loc value in

      check_qualifiers "returns no qualifiers" [] qualifiers;

      check string "captures quoted label text" "hello world"
        (strip_loc label_name);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by thirteen" 13
        resulting_state.loc.coordinate.offset

let succeeds_with_multiple_qualifiers () =
  let startl = zero_point in

  let state : parser_state =
    {
      remaining = BatSubstring.of_string "module:submodule:hello";
      loc = startl;
    }
  in

  match func_label state with
  | Error _ -> fail "func_label should accept multiple qualifiers"
  | Ok (value, resulting_state) ->
      let qualifiers, label_name = strip_loc value in

      check_qualifiers "captures qualifiers in order" [ "module"; "submodule" ]
        qualifiers;

      check string "captures label name" "hello" (strip_loc label_name);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by twenty-two" 22
        resulting_state.loc.coordinate.offset

let succeeds_with_quoted_label_and_qualifier () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "module:'hello world'"; loc = startl }
  in

  match func_label state with
  | Error _ -> fail "func_label should accept a qualified quoted label"
  | Ok (value, resulting_state) ->
      let qualifiers, label_name = strip_loc value in

      check_qualifiers "captures qualifier" [ "module" ] qualifiers;

      check string "captures quoted label text" "hello world"
        (strip_loc label_name);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by twenty" 20
        resulting_state.loc.coordinate.offset

let fails_when_label_is_missing () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "module:"; loc = startl }
  in

  match func_label state with
  | Ok _ -> fail "func_label should require a label after the qualifier"
  | Error `UnexpectedEOF -> ()
  | Error _ -> fail "func_label should fail with UnexpectedEOF"

let fails_when_quoted_label_is_unclosed () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "'hello"; loc = startl }
  in

  match func_label state with
  | Ok _ -> fail "func_label should reject an unclosed quoted label"
  | Error (`ExpectedLowercase _) ->
      (* TODO: We should not fail with this tag on this case.
         However, this requires combining errors in the parser. *)
      ()
  | Error _ -> fail "func_label should fail with ExpectedLowercase"

let fails_when_qualifier_starts_with_uppercase () =
  let startl = zero_point in

  let state : parser_state =
    { remaining = BatSubstring.of_string "Module:hello"; loc = startl }
  in

  match func_label state with
  | Ok _ -> fail "func_label should reject an uppercase qualifier"
  | Error (`ExpectedLowercase _) -> ()
  | Error _ -> fail "func_label should fail with ExpectedLowercase"

let fails_on_empty_input () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = zero_point }
  in

  match func_label state with
  | Ok _ -> fail "func_label should fail on empty input"
  | Error `UnexpectedEOF -> ()
  | Error _ -> fail "func_label should fail with UnexpectedEOF"

let tests =
  [
    test_case "succeeds with atom label" `Quick succeeds_with_atom_label;
    test_case "succeeds with quoted label" `Quick succeeds_with_quoted_label;
    test_case "succeeds with multiple qualifiers" `Quick
      succeeds_with_multiple_qualifiers;
    test_case "succeeds with quoted label and qualifier" `Quick
      succeeds_with_quoted_label_and_qualifier;
    test_case "fails when label is missing" `Quick fails_when_label_is_missing;
    test_case "fails when quoted label is unclosed" `Quick
      fails_when_quoted_label_is_unclosed;
    test_case "fails when qualifier starts with uppercase" `Quick
      fails_when_qualifier_starts_with_uppercase;
    test_case "fails on empty input" `Quick fails_on_empty_input;
  ]
