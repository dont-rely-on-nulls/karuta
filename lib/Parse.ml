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

let ( @> ) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c =
 fun l r -> Fun.compose r l

let ( @>> ) :
      'a 'b 'c 'e.
      ('a -> ('b, 'e) result) ->
      ('b -> ('c, 'e) result) ->
      'a ->
      ('c, 'e) result =
 fun prefix suffix state -> Result.bind (prefix state) suffix

let replace :
      'a 's 'b 'e. ('a -> 'b) -> ('a * 's, 'e) result -> ('b * 's, 'e) result =
 fun f -> Result.map (fun (a, s) -> (f a, s))

let just_state : 'a 'b 'e. ('a * 'b, 'e) result -> ('b, 'e) result =
 fun r -> Result.map (fun (_, s) -> s) r

let ifte :
      'a 'b 'c 'et 'ec.
      ('a -> ('b, 'et) result) ->
      ('b -> ('c, 'ec) result) ->
      ('a -> ('c, 'ec) result) ->
      'a ->
      ('c, 'ec) result =
 fun test consequent alternative state ->
  state
  |> test @> Result.fold ~ok:consequent ~error:(fun _ -> alternative state)

let ( @|| ) l r = ifte l Result.ok r
let ( @&& ) l r = l @> just_state @>> r
let succeed : 'e. (unit, 'e) parser = fun state -> Ok ((), state)

let is_not :
      'a 'errin 'errout.
      ('a, 'errin) parser ->
      ('a -> Location.location -> 'errout) ->
      (unit, 'errout) parser =
 fun p handler ({ loc = startl; _ } as state) ->
  ifte p
    (fun (r, { loc = endl; _ }) -> Error (handler r { startl; endl }))
    succeed state

let is : 'a 'e. ('a, 'e) parser -> ('a, 'e) parser =
 fun p state -> state |> p @>> fun (r, _) -> Ok (r, state)

let mapl f (a, b) = (f a, b)

let maybe : 'a 'el 'er. ('a, 'el) parser -> ('a option, 'er) parser =
 fun p ->
  ifte p (mapl Option.some @> Result.ok) (succeed @> replace @@ Fun.const None)

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
  | Some _ -> Error (`WrongCharacter (loc, c))

let colon = one ':'
let single_quote = one '\''
let comma = one ','
let period = one '.'
let pipe = one '|'
let left_bracket = one '['
let right_bracket = one ']'
let left_curly_brace = one '{'
let right_curly_brace = one '}'

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
        ([> `UnexpectedEOF of Location.t | `WrongCharacter of Location.t * char ]
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
      |> single_quote @>> fun ((), ({ loc = endl; _ } as state)) ->
         Ok
           (Location.add_loc (to_string atom_name) { startl = loc; endl }, state)
  | Some _ | None -> Error (`WrongCharacter (loc, '\''))

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
      match state |> horizontal @>> positive_integer with
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
      ?allow_trailing:bool ->
      (unit, 'e) parser ->
      (unit, 'e) parser ->
      (unit, 'e) parser ->
      ('a, 'e) parser ->
      ('a BatFingerTree.t, 'e) parser =
 fun ?(allow_trailing = false) start_delim separator end_delim item ->
  let open BatFingerTree in
  let trailing =
    if allow_trailing then maybe separator @&& succeed else succeed
  in
  start_delim
  @&& ifte end_delim (mapl (Fun.const empty) @> Result.ok)
  @@ item @> replace singleton
  @>>
  let rec loop (acc, state) =
    state
    |> ifte
         (separator @&& item @> replace (snoc acc))
         loop
         (trailing @&& end_delim @> replace @@ Fun.const acc)
  in
  loop

let func_label :
      'e.
      ( Ast.Expr.func_label,
        ([> `UnexpectedEOF of Location.t
         | `ExpectedLowercase of Location.t
         | `WrongCharacter of Location.t * char ]
         as
         'e) )
      parser =
  list_of succeed colon
    (is @@ colon @&& single_quote @|| atom
    @&& is_not colon (fun () { Location.startl; _ } ->
            `WrongCharacter (startl, ':')))
    atom
  @> replace BatFingerTree.to_list
  @>> fun (qualifiers, state) ->
  state
  |> (quoted_atom @|| atom) @> replace
     @@ fun label_name -> (qualifiers, label_name)

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
