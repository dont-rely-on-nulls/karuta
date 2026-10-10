open Lib.Parser
open Lib.Location
module FT = Lib.FT
open Alcotest

let state_of_string input =
  { remaining = BatSubstring.of_string input; loc = zero_point }

let remaining_string state = BatSubstring.to_string state.remaining

let succeeds input expected_remaining =
  let state = state_of_string input in
  match parser_clause state with
  | Error _ -> failf "parser_clause should parse %S" input
  | Ok (_value, resulting_state) ->
      check string "remaining input" expected_remaining
        (remaining_string resulting_state);

      check int "updates position according to consumed input"
        (String.length input - String.length expected_remaining)
        resulting_state.loc.coordinate.offset

let fails input =
  let state = state_of_string input in
  match parser_clause state with
  | Ok _ -> failf "parser_clause should reject %S" input
  | Error _ -> ()

let succeeds_with_declaration () = succeeds "first()." ""
let succeeds_with_declaration_body () = succeeds "first() :- body()." ""
let succeeds_with_query () = succeeds "first(), second()?" ""
let succeeds_with_single_predicate_query () = succeeds "first()?" ""
let succeeds_with_directive_without_bodies () = succeeds ":- rule()." ""

let succeeds_with_directive_with_one_body () =
  succeeds ":- rule() { fact(). }." ""

let succeeds_with_directive_with_multiple_body_blocks () =
  succeeds ":- rule() { first(). } { second(). }." ""

let succeeds_with_directive_with_multiple_clauses () =
  succeeds ":- rule() { first(). second(). }." ""

let succeeds_with_nested_directives () =
  succeeds ":- outer() { :- inner(). fact(). }." ""

let succeeds_with_whitespace_in_directive () =
  succeeds ":-  rule()  {  fact().  }  ." ""

let succeeds_with_comment_in_directive () =
  succeeds ":- rule() { % a comment\n fact(). }." ""

let succeeds_with_empty_directive_body () = succeeds ":- rule() { }." ""

let succeeds_with_trailing_comment_after_directive () =
  succeeds ":- rule(). % comment\n" ""

let fails_on_empty_input () = fails ""
let fails_on_incomplete_declaration () = fails "first()"
let fails_on_incomplete_query () = fails "first(), "
let fails_on_incomplete_directive () = fails ":-"
let fails_on_directive_without_period () = fails ":- rule()"
let fails_on_unclosed_directive_body () = fails ":- rule() { fact()."

let fails_on_invalid_clause_in_directive_body () =
  fails ":- rule() { first() }."

let tests =
  [
    test_case "succeeds with declaration" `Quick succeeds_with_declaration;
    test_case "succeeds with declaration body" `Quick
      succeeds_with_declaration_body;
    test_case "succeeds with query" `Quick succeeds_with_query;
    test_case "succeeds with single-predicate query" `Quick
      succeeds_with_single_predicate_query;
    test_case "succeeds with directive without bodies" `Quick
      succeeds_with_directive_without_bodies;
    test_case "succeeds with directive with one body" `Quick
      succeeds_with_directive_with_one_body;
    test_case "succeeds with multiple directive body blocks" `Quick
      succeeds_with_directive_with_multiple_body_blocks;
    test_case "succeeds with multiple clauses in a body" `Quick
      succeeds_with_directive_with_multiple_clauses;
    test_case "succeeds with nested directives" `Quick
      succeeds_with_nested_directives;
    test_case "succeeds with whitespace in directive" `Quick
      succeeds_with_whitespace_in_directive;
    test_case "succeeds with comment in directive" `Quick
      succeeds_with_comment_in_directive;
    test_case "succeeds with empty directive body" `Quick
      succeeds_with_empty_directive_body;
    test_case "succeeds with trailing comment after directive" `Quick
      succeeds_with_trailing_comment_after_directive;
    test_case "fails on empty input" `Quick fails_on_empty_input;
    test_case "fails on incomplete declaration" `Quick
      fails_on_incomplete_declaration;
    test_case "fails on incomplete query" `Quick fails_on_incomplete_query;
    test_case "fails on incomplete directive" `Quick
      fails_on_incomplete_directive;
    test_case "fails on directive without period" `Quick
      fails_on_directive_without_period;
    test_case "fails on unclosed directive body" `Quick
      fails_on_unclosed_directive_body;
    test_case "fails on invalid clause in directive body" `Quick
      fails_on_invalid_clause_in_directive_body;
  ]
