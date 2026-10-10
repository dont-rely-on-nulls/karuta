open Lib.Parser
open Lib.Location
module FT = Lib.FT
module Logger = Lib.Logger
open Alcotest

let parse_source source =
  Logger.with_min_level Logger.Level.Unreachable @@ fun () ->
  parse "test.input" source

let succeeds_with_empty_source () =
  let parsed = parse_source "" in
  check int "returns no clauses" 0 (FT.size parsed)

let succeeds_with_whitespace_only_source () =
  let parsed = parse_source "  \t\n  " in
  check int "returns no clauses" 0 (FT.size parsed)

let succeeds_with_single_declaration () =
  let parsed = parse_source "first()." in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_multiple_declarations () =
  let parsed = parse_source "first(). second(). third()." in
  check int "returns three clauses" 3 (FT.size parsed)

let succeeds_with_query () =
  let parsed = parse_source "first()?" in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_multiple_function_query () =
  let parsed = parse_source "first(), second()?" in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_declaration_body () =
  let parsed = parse_source "first() :- body()." in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_directive () =
  let parsed = parse_source ":- rule()." in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_directive_body () =
  let parsed = parse_source ":- rule() { fact(). }." in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_multiple_directive_body_clauses () =
  let parsed = parse_source ":- rule() { first(). second(). }." in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_multiple_directive_body_blocks () =
  let parsed = parse_source ":- rule() { first(). } { second(). }." in
  check int "returns one clause" 1 (FT.size parsed)

let succeeds_with_comments_and_whitespace () =
  let parsed =
    parse_source
      "  first(). % comment\n  second().\n% another comment\n third(). "
  in
  check int "returns three clauses" 3 (FT.size parsed)

let succeeds_with_mixed_top_level_clauses () =
  let parsed =
    parse_source "first(). :- rule() { fact(). }. second() :- body(). third()?"
  in
  check int "returns four clauses" 4 (FT.size parsed)

(* TODO: get rid of this forking mechanism
   This is here because the only way to tell if the parse function
   failed is by checking the exit code.*)
let expect_parse_failure source =
  match Unix.fork () with
  | 0 ->
      ignore (parse_source source);
      Stdlib.exit 0
  | -1 -> fail "could not fork a child process"
  | pid -> (
      match Unix.waitpid [] pid with
      | _, Unix.WEXITED 1 -> ()
      | _, Unix.WEXITED status -> failf "expected exit status 1, got %d" status
      | _, Unix.WSIGNALED signal ->
          failf "parser was terminated by signal %d" signal
      | _, Unix.WSTOPPED signal ->
          failf "parser was stopped by signal %d" signal)

let fails_on_incomplete_declaration () = expect_parse_failure "first()"

let fails_on_unexpected_trailing_character () =
  expect_parse_failure "first(). @"

let fails_on_incomplete_query () = expect_parse_failure "first(), "
let fails_on_incomplete_directive () = expect_parse_failure ":-"

let fails_on_unclosed_directive_body () =
  expect_parse_failure ":- rule() { fact()."

let fails_on_invalid_clause_after_valid_clause () =
  expect_parse_failure "first(). second()"

let tests =
  [
    test_case "succeeds with empty source" `Quick succeeds_with_empty_source;
    test_case "succeeds with whitespace-only source" `Quick
      succeeds_with_whitespace_only_source;
    test_case "succeeds with single declaration" `Quick
      succeeds_with_single_declaration;
    test_case "succeeds with multiple declarations" `Quick
      succeeds_with_multiple_declarations;
    test_case "succeeds with query" `Quick succeeds_with_query;
    test_case "succeeds with multiple-function query" `Quick
      succeeds_with_multiple_function_query;
    test_case "succeeds with declaration body" `Quick
      succeeds_with_declaration_body;
    test_case "succeeds with directive" `Quick succeeds_with_directive;
    test_case "succeeds with directive body" `Quick succeeds_with_directive_body;
    test_case "succeeds with multiple clauses in directive body" `Quick
      succeeds_with_multiple_directive_body_clauses;
    test_case "succeeds with multiple directive body blocks" `Quick
      succeeds_with_multiple_directive_body_blocks;
    test_case "succeeds with comments and whitespace" `Quick
      succeeds_with_comments_and_whitespace;
    test_case "succeeds with mixed top-level clauses" `Quick
      succeeds_with_mixed_top_level_clauses;
    test_case "fails on incomplete declaration" `Quick
      fails_on_incomplete_declaration;
    test_case "fails on unexpected trailing character" `Quick
      fails_on_unexpected_trailing_character;
    test_case "fails on incomplete query" `Quick fails_on_incomplete_query;
    test_case "fails on incomplete directive" `Quick
      fails_on_incomplete_directive;
    test_case "fails on unclosed directive body" `Quick
      fails_on_unclosed_directive_body;
    test_case "fails on invalid clause after valid clause" `Quick
      fails_on_invalid_clause_after_valid_clause;
  ]
