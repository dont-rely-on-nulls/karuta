open Lib.Parser
open Lib.Location
module Ast = Lib.Ast
open Alcotest

let succeeds_with_integer () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "123 rest"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse an integer"
  | Ok (value, resulting_state) ->
      check bool "parses integer expression" true
        (strip_loc value = Ast.Expr.integer 123);

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by three" 3 resulting_state.loc.pos_cnum

let succeeds_with_variable () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "Hello rest"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse a variable"
  | Ok (value, resulting_state) ->
      check bool "parses variable expression" true
        (strip_loc value = Ast.Expr.variable "Hello");

      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let succeeds_with_line_comment () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "% comment\n123"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should skip a line comment"
  | Ok (value, resulting_state) ->
      check bool "parses integer after comment" true
        (strip_loc value = Ast.Expr.integer 123);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position past comment and integer" 13
        resulting_state.loc.pos_cnum

let succeeds_with_expression_comment () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "#%123\n456"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should skip an expression comment"
  | Ok (value, resulting_state) ->
      check bool "parses expression after comment" true
        (strip_loc value = Ast.Expr.integer 456);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position across comment and expression" 9
        resulting_state.loc.pos_cnum

let succeeds_with_nested_lists () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[1,[2,3]]"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse nested lists"
  | Ok (value, resulting_state) ->
      (match strip_loc value with
      | Ast.Expr.Cons _ -> ()
      | _ -> fail "expected a non-empty outer list");

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by ten" 9 resulting_state.loc.pos_cnum

let succeeds_with_list_whitespace () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[ 1 , 2 ]"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should accept whitespace inside lists"
  | Ok (value, resulting_state) ->
      (match strip_loc value with
      | Ast.Expr.Cons _ -> ()
      | _ -> fail "expected a non-empty list expression");

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by nine" 9 resulting_state.loc.pos_cnum

let succeeds_with_functor () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "sum(1,2) rest"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse a function call"
  | Ok (_value, resulting_state) ->
      check string "leaves remaining input" " rest"
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by eight" 8 resulting_state.loc.pos_cnum

let succeeds_with_empty_functor () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "sum()"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse a function call with no arguments"
  | Ok (_value, resulting_state) ->
      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by five" 5 resulting_state.loc.pos_cnum

let succeeds_with_bracket_functor () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "sum[1,2]"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse a bracket-style function call"
  | Ok (_value, resulting_state) ->
      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by eight" 8 resulting_state.loc.pos_cnum

let succeeds_with_nested_functor () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "sum(1,product(2,3))"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse nested function calls"
  | Ok (_value, resulting_state) ->
      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by nineteen" 19 resulting_state.loc.pos_cnum

let succeeds_with_functor_inside_list () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[sum(1,2),3]"; loc = startl }
  in

  match expr state with
  | Error _ -> fail "expr should parse a function call inside a list"
  | Ok (value, resulting_state) ->
      (match strip_loc value with
      | Ast.Expr.Cons _ -> ()
      | _ -> fail "expected a non-empty list expression");

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "advances position by twelve" 12 resulting_state.loc.pos_cnum

let fails_on_empty_input () =
  let state : parser_state =
    { remaining = BatSubstring.of_string ""; loc = half_dummy }
  in

  match expr state with
  | Ok _ -> fail "expr should fail on empty input"
  | Error _ -> ()

let fails_on_incomplete_list () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "[1,2"; loc = startl }
  in

  match expr state with
  | Ok _ -> fail "expr should reject a list without a closing delimiter"
  | Error _ -> ()

let fails_on_incomplete_function_call () =
  let startl = zero_half in

  let state : parser_state =
    { remaining = BatSubstring.of_string "sum(1,2"; loc = startl }
  in

  match expr state with
  | Ok _ ->
      fail "expr should reject a function call without a closing delimiter"
  | Error _ -> ()

let tests =
  [
    test_case "succeeds with integer" `Quick succeeds_with_integer;
    test_case "succeeds with variable" `Quick succeeds_with_variable;
    test_case "succeeds with line comment" `Quick succeeds_with_line_comment;
    test_case "succeeds with expression comment" `Quick
      succeeds_with_expression_comment;
    test_case "succeeds with nested lists" `Quick succeeds_with_nested_lists;
    test_case "succeeds with list whitespace" `Quick
      succeeds_with_list_whitespace;
    test_case "succeeds with functor" `Quick succeeds_with_functor;
    test_case "succeeds with empty functor" `Quick succeeds_with_empty_functor;
    test_case "succeeds with bracket functor" `Quick
      succeeds_with_bracket_functor;
    test_case "succeeds with nested functor call" `Quick
      succeeds_with_nested_functor;
    test_case "succeeds with functor call inside list" `Quick
      succeeds_with_functor_inside_list;
    test_case "fails on empty input" `Quick fails_on_empty_input;
    test_case "fails on incomplete list" `Quick fails_on_incomplete_list;
    test_case "fails on incomplete function call" `Quick
      fails_on_incomplete_function_call;
  ]
