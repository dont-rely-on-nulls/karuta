open Lib.Parser
open Lib.Location
module FT = Lib.FT
module Ast = Lib.Ast
open Alcotest

let parse_func input =
  let state : parser_state =
    { remaining = BatSubstring.of_string input; loc = zero_half }
  in
  match func state with
  | Ok (value, _) -> value
  | Error _ -> failf "could not parse initial predicate from %S" input

let declaration_after head input =
  { remaining = BatSubstring.of_string input; loc = head.loc.endl }
  |> declaration head

let succeeds_without_body () =
  let head = parse_func "first()" in

  match declaration_after head ". rest" with
  | Error _ -> fail "declaration should accept a head without a body"
  | Ok (value, resulting_state) ->
      let decl = strip_loc value in

      check bool "preserves declaration head" true
        (decl.Ast.ParserClause.head = head.content);

      check int "returns an empty body" 0 (FT.size decl.body);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "consumes the period" 8 resulting_state.loc.pos_cnum

let succeeds_with_one_body_element () =
  let head = parse_func "first()" in

  match declaration_after head ":- body(). rest" with
  | Error _ -> fail "declaration should accept one body element"
  | Ok (value, resulting_state) ->
      let decl = strip_loc value in

      check bool "preserves declaration head" true
        (decl.Ast.ParserClause.head = head.content);

      check int "returns one body element" 1 (FT.size decl.body);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "consumes declaration through period" 17
        resulting_state.loc.pos_cnum

let succeeds_with_multiple_body_elements () =
  let head = parse_func "first()" in

  match declaration_after head ":- one(), two()." with
  | Error _ -> fail "declaration should accept multiple body elements"
  | Ok (value, resulting_state) ->
      let decl = strip_loc value in

      check bool "preserves declaration head" true
        (decl.Ast.ParserClause.head = head.content);

      check int "returns two body elements" 2 (FT.size decl.body);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "consumes declaration through period" 23
        resulting_state.loc.pos_cnum

let succeeds_with_whitespace () =
  let head = parse_func "first()" in

  match declaration_after head ":-  body()  ." with
  | Error _ -> fail "declaration should accept whitespace around its body"
  | Ok (value, resulting_state) ->
      let decl = strip_loc value in

      check int "returns one body element" 1 (FT.size decl.body);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "consumes all characters" 20 resulting_state.loc.pos_cnum

let succeeds_with_line_comment () =
  let head = parse_func "first()" in

  match declaration_after head ":- % comment\n body()." with
  | Error _ -> fail "declaration should accept a line comment"
  | Ok (value, resulting_state) ->
      let decl = strip_loc value in

      check int "returns one body element" 1 (FT.size decl.body);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining)

let fails_without_period () =
  let head = parse_func "first()" in

  match declaration_after head ":- body()" with
  | Ok _ -> fail "declaration should require a terminating period"
  | Error _ -> ()

let fails_when_body_element_is_missing () =
  let head = parse_func "first()" in

  match declaration_after head ":- ." with
  | Ok _ -> fail "declaration should reject a missing body element"
  | Error _ -> ()

let fails_when_comma_is_not_followed_by_a_predicate () =
  let head = parse_func "first()" in

  match declaration_after head ":- one(), ." with
  | Ok _ -> fail "declaration should reject a missing predicate after a comma"
  | Error _ -> ()

let fails_without_head_terminator () =
  let head = parse_func "first()" in

  match declaration_after head "" with
  | Ok _ -> fail "declaration should require a terminating period"
  | Error _ -> ()

let tests =
  [
    test_case "succeeds without body" `Quick succeeds_without_body;
    test_case "succeeds with one body element" `Quick
      succeeds_with_one_body_element;
    test_case "succeeds with multiple body elements" `Quick
      succeeds_with_multiple_body_elements;
    test_case "succeeds with whitespace" `Quick succeeds_with_whitespace;
    test_case "succeeds with line comment" `Quick succeeds_with_line_comment;
    test_case "fails without period" `Quick fails_without_period;
    test_case "fails when body element is missing" `Quick
      fails_when_body_element_is_missing;
    test_case "fails when comma is not followed by a predicate" `Quick
      fails_when_comma_is_not_followed_by_a_predicate;
    test_case "fails without head terminator" `Quick
      fails_without_head_terminator;
  ]
