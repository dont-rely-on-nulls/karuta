open Lib.Parser
open Lib.Location
module FT = Lib.FT
open Alcotest

let succeeds_repeatedly () =
  let startl = zero_point in

  let p : (char, string) parser =
   fun state ->
    match BatSubstring.first state.remaining with
    | None -> Error "no more input"
    | Some c ->
        let remaining = BatSubstring.triml 1 state.remaining in
        let loc = dummy_coord_to_point @@ step 1 state.loc.coordinate in
        Ok (c, { remaining; loc })
  in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abc"; loc = startl }
  in

  match plus p state with
  | Error error -> failf "plus should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (list char) "parses all characters" [ 'a'; 'b'; 'c' ]
        (FT.to_list value);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "updates position" 3 resulting_state.loc.coordinate.offset

let fails_when_parser_fails_immediately () =
  let p : (char, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = zero_point }
  in

  match plus p state with
  | Ok _ -> fail "plus should fail when the parser fails initially"
  | Error error -> check string "propagates parser error" "p failed" error

let succeeds_with_one_occurrence () =
  let startl = zero_point in

  let p : (char, string) parser =
   fun state ->
    match BatSubstring.first state.remaining with
    | Some 'a' ->
        let remaining = BatSubstring.triml 1 state.remaining in
        Ok
          ( 'a',
            {
              remaining;
              loc = dummy_coord_to_point @@ step 1 state.loc.coordinate;
            } )
    | _ -> Error "expected a"
  in

  let state : parser_state =
    { remaining = BatSubstring.of_string "a"; loc = startl }
  in

  match plus p state with
  | Error error -> failf "plus should succeed once, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (list char) "returns one parsed value" [ 'a' ] (FT.to_list value);

      check string "consumes input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "updates position" 1 resulting_state.loc.coordinate.offset

let tests =
  [
    test_case "succeeds repeatedly" `Quick succeeds_repeatedly;
    test_case "fails when parser fails initially" `Quick
      fails_when_parser_fails_immediately;
    test_case "succeeds with one occurrence" `Quick succeeds_with_one_occurrence;
  ]
