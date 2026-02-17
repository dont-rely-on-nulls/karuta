open Lexing
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ ->
      Logger.simply_unreachable "env: parser is broken";
      assert false

let state = Fun.compose I.current_state_number env

let get input checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      (pos1, pos2) |> E.extract input |> E.sanitize |> E.compress
  | None ->
      Logger.simply_unreachable "get: parser is broken";
      exit 1

let fail input buffer (checkpoint : _ I.checkpoint) =
  let pos1, pos2 = E.last buffer in
  let open Location in
  let loc = { startl = to_t pos1; endl = to_t pos2 } in
  let msg = ParserMessages.message (state checkpoint) in
  let msg = E.expand (get input checkpoint) msg in
  Logger.error loc msg;
  exit 1

let parse_loop (input : string) (lexbuf : lexbuf) =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.file lexbuf.lex_curr_p in
  I.loop_handle Fun.id (fail input buffer) supplier checkpoint

let parse (filepath : string) : Ast.ParserClause.t list =
  let input, lexbuf = L.read filepath in
  try parse_loop input lexbuf
  with Lexer.SyntaxError (loc, message) ->
    Logger.error loc message;
    exit 1

type parser_state = { remaining : BatSubstring.t; loc : Location.t }
type ('a, 'e) parser = parser_state -> ('a * parser_state, 'e) result

let bind f (p : ('a, 'e) parser) (state : parser_state) =
  match p state with Error _ as error -> error | Ok r -> f r

open BatSubstring

let stride previous current = size previous - size current

let horizontal_whitespace :
      'e.
      (unit, ([> `ExpectedHorizontalWhitespace of Location.t ] as 'e)) parser =
 fun { remaining = current; loc } ->
  let remaining = dropl (function ' ' | '\t' -> true | _ -> false) current in
  match stride current remaining with
  | 0 -> Error (`ExpectedHorizontalWhitespace loc)
  | dropped -> Ok ((), { remaining; loc = Location.step dropped loc })

let newline : 'e. (unit, ([> `ExpectedNewline of Location.t ] as 'e)) parser =
 fun { remaining = current; loc } ->
  match
    if is_prefix "\n" current then Some 1
    else if is_prefix "\r\n" current then Some 2
    else None
  with
  | Some len ->
      let remaining = triml len current in
      Ok ((), { remaining; loc = loc |> Location.step len |> Location.jump })
  | None -> Error (`ExpectedNewline loc)

let horizontal :
      'e.
      ( unit,
        ([> `UnexpectedEOF of Location.t | `UnexpectedNewline of Location.t ]
         as
         'e) )
      parser =
 fun { remaining; loc } ->
  match first remaining with
  | None -> Error (`UnexpectedEOF loc)
  | Some c when c = '\n' || c = '\r' -> Error (`UnexpectedNewline loc)
  | Some _ ->
      Ok ((), { remaining = triml 1 remaining; loc = Location.step 1 loc })

let rec skip_whitespace : 'e. (unit, 'e) parser =
 fun state ->
  match horizontal_whitespace state with
  | Ok (_, state) -> skip_whitespace state
  | Error (`ExpectedHorizontalWhitespace _) -> (
      match newline state with
      | Ok (_, state) -> skip_whitespace state
      | Error (`ExpectedNewline _) -> Ok ((), state))

let variable :
      'e.
      ( string Location.with_location,
        ([> `ExpectedUppercaseOrUnderscore of Location.t
         | `UnexpectedEOF of Location.t ]
         as
         'e) )
      parser =
 fun { remaining = current; loc } ->
  let variable_start = function
    | '_' -> true
    | c when BatChar.is_uppercase c -> true
    | _ -> false
  in
  let variable_character c =
    BatChar.is_letter c || BatChar.is_digit c || c = '_'
  in
  match first current with
  | None -> Error (`UnexpectedEOF loc)
  | Some c when variable_start c ->
      let variable, remaining = splitl variable_character current in
      let next_loc = Location.step (stride current remaining) loc in
      Ok
        ( Location.add_loc (to_string variable) { startl = loc; endl = next_loc },
          { remaining; loc = next_loc } )
  | Some _ -> Error (`ExpectedUppercaseOrUnderscore loc)

let integer :
      'e.
      ( int Location.with_location,
        ([> `NotADigit of Location.t | `UnexpectedEOF of Location.t ] as 'e) )
      parser =
 fun ({ remaining = current; loc } as state) ->
  let positive_integer ((), { remaining = current; loc = after_minus }) =
    match size current with
    | 0 -> Error (`UnexpectedEOF after_minus)
    | _ -> (
        let digits, remaining = splitl BatChar.is_digit current in
        let endl = Location.step (stride current remaining) after_minus in
        match to_string digits with
        | "" -> Error (`NotADigit after_minus)
        | digits ->
            Ok
              ( Location.add_loc (int_of_string digits) { startl = loc; endl },
                { remaining; loc = endl } ))
  in
  match first current with
  | Some '-' -> (
      match bind positive_integer horizontal state with
      | Ok (result, next_state) -> Ok (Location.fmap Int.neg result, next_state)
      | Error (`NotADigit _ | `UnexpectedEOF _) as err -> err
      | Error (`UnexpectedNewline endl) ->
          Logger.unreachable { startl = loc; endl }
            "This should never happen since we already saw the minus character \
             at the start of the state";
          exit 1)
  | Some _ -> positive_integer ((), state)
  | None -> Error (`UnexpectedEOF loc)

let parse_file : 'e. (Ast.ParserClause.t list, 'e) parser =
 fun _ -> failwith "TODO"

let parse' (filepath : string) (source : string) =
  match
    parse_file
      {
        remaining = BatSubstring.all source;
        loc = { pos_fname = filepath; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
      }
  with
  | Ok (parsed, _) -> Ok parsed
  | Error _ as err -> err
