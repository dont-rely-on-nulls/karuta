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

  match star p state with
  | Error error -> failf "star should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (list char) "parses all characters" [ 'a'; 'b'; 'c' ]
        (FT.to_list value);

      check string "consumes all input" ""
        (BatSubstring.to_string resulting_state.remaining);

      check int "updates position" 3 resulting_state.loc.coordinate.offset

let succeeds_when_parser_fails_immediately () =
  let p : (char, string) parser = fun _state -> Error "p failed" in

  let state : parser_state =
    { remaining = BatSubstring.of_string "hello"; loc = zero_point }
  in

  match star p state with
  | Error error ->
      failf "star should succeed with an empty tree, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (list char) "returns an empty tree" [] (FT.to_list value);

      check string "preserves remaining input" "hello"
        (BatSubstring.to_string resulting_state.remaining);

      check int "preserves position" state.loc.coordinate.offset
        resulting_state.loc.coordinate.offset

let stops_after_failure () =
  let startl = zero_point in

  let p : (char, string) parser =
   fun state ->
    match BatSubstring.first state.remaining with
    | Some ('a' as c) | Some ('b' as c) ->
        let remaining = BatSubstring.triml 1 state.remaining in
        Ok
          ( c,
            {
              remaining;
              loc = dummy_coord_to_point @@ step 1 state.loc.coordinate;
            } )
    | _ -> Error "no match"
  in

  let state : parser_state =
    { remaining = BatSubstring.of_string "abx"; loc = startl }
  in

  match star p state with
  | Error error -> failf "star should succeed, but returned: %s" error
  | Ok (value, resulting_state) ->
      check (list char) "keeps successful results" [ 'a'; 'b' ]
        (FT.to_list value);

      check string "leaves unmatched input" "x"
        (BatSubstring.to_string resulting_state.remaining);

      check int "updates position for successful parses" 2
        resulting_state.loc.coordinate.offset

let tests =
  [
    test_case "succeeds repeatedly" `Quick succeeds_repeatedly;
    test_case "succeeds when parser fails immediately" `Quick
      succeeds_when_parser_fails_immediately;
    test_case "stops after failure" `Quick stops_after_failure;
  ]
