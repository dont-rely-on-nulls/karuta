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

let ( @> ) prefix suffix state = Result.bind (prefix state) suffix

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

let one c { remaining; loc } =
  match first remaining with
  | None -> Error (`UnexpectedEOF loc)
  | Some found when found = c ->
      Ok
        ( (),
          {
            remaining = triml 1 remaining;
            loc =
              (Location.step 1 loc |> if c = '\n' then Location.jump else Fun.id);
          } )
  | Some _ -> Error (`WrongCharacter loc)

let colon = one ':'
let single_quote = one '\''
let comma = one ','
let period = one '.'
let pipe = one '|'
let left_bracket = one '['
let right_bracket = one ']'
let left_curly_brace = one '{'
let right_curly_brace = one '}'
let empty state = Ok ((), state)

let rec skip_whitespace : 'e. (unit, 'e) parser =
 fun state ->
  match horizontal_whitespace state with
  | Ok (_, state) -> skip_whitespace state
  | Error (`ExpectedHorizontalWhitespace _) -> (
      match newline state with
      | Ok (_, state) -> skip_whitespace state
      | Error (`ExpectedNewline _) -> Ok ((), state))

let ident_like is_start is_character fallthrough { remaining = current; loc } =
  match first current with
  | None -> Error (`UnexpectedEOF loc)
  | Some c when is_start c ->
      let atom, remaining = splitl is_character current in
      let next_loc = Location.step (stride current remaining) loc in
      Ok
        ( Location.add_loc (to_string atom) { startl = loc; endl = next_loc },
          { remaining; loc = next_loc } )
  | Some _ -> Error (fallthrough loc)

let atom :
      'e.
      ( string Location.with_location,
        ([> `UnexpectedEOF of Location.t | `ExpectedLowercase of Location.t ]
         as
         'e) )
      parser =
  ident_like BatChar.is_lowercase
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_' || c = '-')
    (fun loc -> `ExpectedLowercase loc)

let variable :
      'e.
      ( string Location.with_location,
        ([> `ExpectedUppercaseOrUnderscore of Location.t
         | `UnexpectedEOF of Location.t ]
         as
         'e) )
      parser =
  ident_like
    (function
      | '_' -> true | c when BatChar.is_uppercase c -> true | _ -> false)
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_')
    (fun loc -> `ExpectedUppercaseOrUnderscore loc)

let quoted_atom :
      'e.
      ( string Location.with_location,
        ([> `UnexpectedEOF of Location.t
         | `ExpectedSingleQuote of Location.t
         | `WrongCharacter of Location.t ]
         as
         'e) )
      parser =
 fun { remaining = current; loc } ->
  match getc current with
  | Some ('\'', remaining) ->
      let atom_name, remaining =
        splitl
          (function '\'' -> true | c when c <> '\n' -> true | _ -> false)
          remaining
      in
      let next_loc = Location.step (stride current remaining) loc in
      { remaining; loc = next_loc }
      |> single_quote @> fun ((), ({ loc = endl; _ } as state)) ->
         Ok
           (Location.add_loc (to_string atom_name) { startl = loc; endl }, state)
  | Some _ | None -> Error (`ExpectedSingleQuote loc)

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
      match state |> horizontal @> positive_integer with
      | Ok (result, next_state) -> Ok (Location.fmap Int.neg result, next_state)
      | Error (`NotADigit _ | `UnexpectedEOF _) as err -> err
      | Error (`UnexpectedNewline endl) ->
          Logger.unreachable { startl = loc; endl }
            "This should never happen since we already saw the minus character \
             at the start of the state";
          exit 1)
  | Some _ -> positive_integer ((), state)
  | None -> Error (`UnexpectedEOF loc)

let list_of :
      'a 'e.
      (unit, 'e) parser ->
      (unit, 'e) parser ->
      (unit, 'e) parser ->
      ('a, 'e) parser ->
      ('a BatFingerTree.t, 'e) parser =
 fun start_delim separator end_delim item ->
  start_delim @> fun ((), after_start) ->
  after_start
  |>
  let open BatFingerTree in
  (* TODO: This is too greedy. Rethink the grammar. *)
  let rec loop acc =
    item @> fun (elem, state) ->
    let acc = BatFingerTree.snoc acc elem in
    match separator state with
    | Error _ -> state |> end_delim @> fun ((), state) -> Ok (acc, state)
    | Ok ((), state) -> state |> loop acc
  in
  loop empty

(*
   let func_label : 'e. (Ast.Expr.func_label, 'e) parser =
     list_of empty colon empty
*)

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
