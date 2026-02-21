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

(* New Parser *)

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
let ( @&& ) l r = l @> Result.map snd @>> r
let return : 'a 'e. 'a -> ('a, 'e) parser = fun v state -> Ok (v, state)
let succeed : 'e. (unit, 'e) parser = return ()

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
let injl v a = (v, a)

let maybe : 'a 'el 'er. ('a, 'el) parser -> ('a option, 'er) parser =
 fun p -> ifte p (mapl Option.some @> Result.ok) (return None)

let capture :
      'a 'b 'e.
      ('a -> ('b, 'e) parser) ->
      'a * parser_state ->
      ('b * parser_state, 'e) result =
 fun f (result, state) -> f result state

let star : 'a 'e 'none. ('a, 'e) parser -> ('a BatFingerTree.t, 'none) parser =
 fun p state ->
  state
  |>
  let rec loop acc =
    ifte p
      (capture @@ fun elem -> loop (BatFingerTree.snoc acc elem))
      (return acc)
  in
  loop BatFingerTree.empty

let plus p =
  p @>> capture
  @@ fun first -> star p @> replace @@ Fun.flip BatFingerTree.cons first

open BatSubstring

let stride previous current = size previous - size current

let some : 'e. (unit, ([> `UnexpectedEOF ] as 'e)) parser = function
  | { remaining; loc } -> (
      match first remaining with
      | None -> Error `UnexpectedEOF
      | Some _ ->
          Ok ((), { remaining = triml 1 remaining; loc = Location.step 1 loc }))

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
        ([> `UnexpectedEOF | `UnexpectedNewline of Location.t ] as 'e) )
      parser =
 fun { remaining; loc } ->
  match first remaining with
  | None -> Error `UnexpectedEOF
  | Some c when c = '\n' || c = '\r' -> Error (`UnexpectedNewline loc)
  | Some _ ->
      Ok ((), { remaining = triml 1 remaining; loc = Location.step 1 loc })

let one c { remaining; loc } =
  match first remaining with
  | None -> Error `UnexpectedEOF
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
let percent = one '%'
let hash = one '#'
let single_quote = one '\''
let comma = one ','
let period = one '.'
let pipe = one '|'
let question_mark = one '?'
let left_paren = one '('
let right_paren = one ')'
let left_bracket = one '['
let right_bracket = one ']'
let left_curly_brace = one '{'
let right_curly_brace = one '}'
let holds = one ':' @&& one '-'

let rec skip_line : 'e. (unit, 'e) parser =
 fun state -> state |> newline @|| ifte some (snd @> skip_line) succeed

let rec skip_whitespace : 'e. (unit, 'e) parser =
 fun state ->
  let loop = snd @> skip_whitespace in
  state |> ifte horizontal_whitespace loop @@ ifte newline loop succeed

let ident_like is_start is_character fallthrough { remaining = current; loc } =
  match first current with
  | None -> Error `UnexpectedEOF
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
        ([> `UnexpectedEOF | `ExpectedLowercase of Location.t ] as 'e) )
      parser =
  ident_like BatChar.is_lowercase
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_' || c = '-')
    (fun loc -> `ExpectedLowercase loc)

let variable :
      'e.
      ( string Location.with_location,
        ([> `ExpectedUppercaseOrUnderscore of Location.t | `UnexpectedEOF ]
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
        ([> `UnexpectedEOF | `WrongCharacter of Location.t * char ] as 'e) )
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
        ([> `NotADigit of Location.t | `UnexpectedEOF ] as 'e) )
      parser =
 fun ({ remaining = current; loc } as state) ->
  let positive_integer ((), { remaining = current; loc = after_minus }) =
    match size current with
    | 0 -> Error `UnexpectedEOF
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
      | Error (`NotADigit _ | `UnexpectedEOF) as err -> err
      | Error (`UnexpectedNewline endl) ->
          Logger.unreachable { startl = loc; endl }
            "This should never happen since we already saw the minus character \
             at the start of the state";
          exit 1)
  | Some _ -> positive_integer ((), state)
  | None -> Error `UnexpectedEOF

let list_of :
      'a 'e.
      ?allow_trailing:bool ->
      start_delim:(unit, 'e) parser ->
      separator:(unit, 'e) parser ->
      end_delim:(unit, 'e) parser ->
      ('a, 'e) parser ->
      ('a BatFingerTree.t, 'e) parser =
 fun ?(allow_trailing = false) ~start_delim ~separator ~end_delim item ->
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
      ( Ast.Expr.func_label Location.with_location,
        ([> `UnexpectedEOF
         | `ExpectedLowercase of Location.t
         | `WrongCharacter of Location.t * char ]
         as
         'e) )
      parser =
 fun ({ loc = startl; _ } as state) ->
  state
  |> star (atom @>> capture @@ fun a -> colon @&& return a)
     @> replace BatFingerTree.to_list
     @>> fun (qualifiers, state) ->
     state
     |> (quoted_atom @|| atom)
        @>> fun (label_name, ({ loc = endl; _ } as state)) ->
        Ok (Location.add_loc (qualifiers, label_name) { startl; endl }, state)

type expr_errors =
  [ `ExpectedLowercase of Location.t
  | `UnexpectedEOF
  | `WrongCharacter of Location.t * char ]

let rec expr : 'e. (Ast.Expr.t, ([> expr_errors ] as 'e)) parser =
 fun state ->
  state
  |> skip_whitespace_and_comments
     @&& ifte integer (mapl (Location.fmap Ast.Expr.integer) @> Result.ok)
     @@ ifte variable (mapl (Location.fmap Ast.Expr.variable) @> Result.ok)
     @@ ifte func (mapl (Location.fmap Ast.Expr.functorr) @> Result.ok)
     @@ list

and skip_whitespace_and_comments : 'e. (unit, ([> expr_errors ] as 'e)) parser =
 fun state ->
  state
  |> skip_whitespace
     @&& ifte percent (snd @> skip_line @&& skip_whitespace_and_comments)
     @@ ifte (hash @&& percent)
          (snd @> expr @&& skip_whitespace_and_comments)
          succeed

and list : 'e. (Ast.Expr.t, ([> expr_errors ] as 'e)) parser =
 fun ({ loc = startl; _ } as state) ->
  let prefix_elements =
    list_of ~start_delim:left_bracket
      ~separator:(skip_whitespace_and_comments @&& comma)
      ~end_delim:(skip_whitespace_and_comments @&& is right_bracket @|| is pipe)
    @@ skip_whitespace_and_comments @&& expr
  in
  let build_cons startl endl prefix (tail : Ast.Expr.t) : Ast.Expr.t =
    BatFingerTree.fold_right
      (fun acc elem ->
        Location.add_loc
          (Ast.Expr.Cons (elem, acc))
          { startl = elem.loc.startl; endl })
      tail prefix
    |> fun { loc; content } ->
    { Location.content; loc = { startl; endl = loc.endl } }
  in
  let nil startl =
    right_bracket @&& fun ({ loc = endl; _ } as state) ->
    Ok
      ( { Location.content = Ast.Expr.Nil; loc = { Location.startl; endl } },
        state )
  in
  state
  |> prefix_elements @>> fun (prefix, state) ->
     if BatFingerTree.is_empty prefix then state |> right_bracket @&& nil startl
     else
       state
       |> ifte pipe
            (snd @> skip_whitespace_and_comments @&& expr
            @>> fun (tail, state) ->
            state
            |> skip_whitespace_and_comments @&& right_bracket @>> snd
               @> fun ({ loc = endl; _ } as state) ->
               Ok (build_cons startl endl prefix tail, state))
          @@ nil startl
          @>> fun (tail, ({ loc = endl; _ } as state)) ->
          Ok (build_cons startl endl prefix tail, state)

and func :
      'e.
      (Ast.Expr.func Location.with_location, ([> expr_errors ] as 'e)) parser =
 fun ({ loc = startl; _ } as state) ->
  let single_element = skip_whitespace_and_comments @&& expr in
  let karuta_elements =
    list_of ~start_delim:left_bracket
      ~separator:(skip_whitespace_and_comments @&& comma)
      ~end_delim:(skip_whitespace_and_comments @&& right_bracket)
    @@ single_element
  in
  let prolog_elements =
    list_of ~start_delim:left_paren
      ~separator:(skip_whitespace_and_comments @&& comma)
      ~end_delim:(skip_whitespace_and_comments @&& right_paren)
    @@ single_element
  in
  state
  |> func_label @>> fun (label, state) ->
     state
     |> ifte
          (is left_bracket @|| is left_paren)
          (snd
          @> (prolog_elements @|| karuta_elements)
          @>> fun (args, ({ loc = endl; _ } as state)) ->
          Ok
            ( Location.add_loc
                (Ast.Expr.func label.content (BatFingerTree.to_list args))
                { startl; endl },
              state ))
          (injl (Location.fmap Ast.Expr.atom label) @> Result.ok)

(* TODO: for the composites below, the location around the result should encompass
   the entire text, not just the first element *)

let query :
      'e.
      Ast.Expr.func Location.with_location ->
      ( Ast.Expr.func Location.with_location list Location.with_location,
        ([> expr_errors ] as 'e) )
      parser =
 fun first_element ->
  (ifte question_mark (snd @> return [ first_element ])
  @@
  let space_comma : (unit, 'e) parser =
    skip_whitespace_and_comments @&& comma
  in
  list_of ~start_delim:space_comma ~separator:space_comma
    ~end_delim:(skip_whitespace_and_comments @&& question_mark)
    func
  @> replace @@ BatFingerTree.to_list @> List.cons first_element)
  @> replace
  @@ fun l -> Location.add_loc l first_element.loc

let declaration :
      'e.
      Ast.Expr.func Location.with_location ->
      ( Ast.ParserClause.decl Location.with_location,
        ([> expr_errors ] as 'e) )
      parser =
 fun { Location.content = head; loc } ->
  (list_of ~start_delim:holds
     ~separator:(skip_whitespace_and_comments @&& comma)
     ~end_delim:(skip_whitespace_and_comments @&& period)
     (skip_whitespace_and_comments @&& func)
  @|| period @&& return BatFingerTree.empty)
  @> replace @@ BatFingerTree.to_list
  @> fun body -> Location.add_loc { Ast.ParserClause.head; body } loc

let rec parser_clause :
      'e. (Ast.ParserClause.t, ([> expr_errors ] as 'e)) parser =
 fun state ->
  print_endline "Parsing a clause";
  state
  |> (ifte holds
        (snd @> skip_whitespace_and_comments @&& directive @> replace
        @@ Location.fmap Ast.ParserClause.directive)
     @@ func @>> capture
     @@ fun first ->
     skip_whitespace_and_comments
     @&& ifte (query first)
           (mapl (Location.fmap Ast.ParserClause.query) @> Result.ok)
           (declaration first @> replace
           @@ Location.fmap Ast.ParserClause.declaration))
     @>> capture
     @@ fun ret -> skip_whitespace_and_comments @&& return ret

and directive :
      'e.
      ( (Ast.Expr.func Location.with_location * Ast.ParserClause.t list)
        Location.with_location,
        ([> expr_errors ] as 'e) )
      parser =
 fun state ->
  state
  |> func @>> fun (header, state) ->
     state
     |> skip_whitespace_and_comments @&& left_curly_brace @&& top_level
        @>> capture
        @@ fun body ->
        right_curly_brace @&& skip_whitespace_and_comments @&& period
        @&& skip_whitespace_and_comments @&& return
        @@ Location.add_loc (header, body) header.loc

and top_level : 'e. (Ast.ParserClause.t list, ([> expr_errors ] as 'e)) parser =
 fun state ->
  state
  |> plus (skip_whitespace_and_comments @&& parser_clause)
     @> replace @@ BatFingerTree.to_list

let parse' (filepath : string) (source : string) =
  match
    {
      remaining = BatSubstring.all source;
      loc = { pos_fname = filepath; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    }
    |> top_level @>> capture
       @@ fun file ->
       is_not some (fun () loc -> `ExpectedEOF (file, loc)) @&& return file
  with
  | Ok (parsed, _) -> parsed
  | Error e ->
      (match e with
      | `ExpectedEOF (_, loc) ->
          Logger.error loc "Expected the file to end, but it continued"
      | `ExpectedLowercase loc ->
          Logger.error (Location.double loc)
            "Expected a lower case letter, but got something else"
      | `UnexpectedEOF ->
          Logger.simply_error "File ended, but we expected it to continue"
      | `WrongCharacter (loc, expected_char) ->
          Logger.error (Location.double loc)
          @@ "We were expecting a " ^ Char.escaped expected_char
          ^ ", but got this instead.");
      exit 1
