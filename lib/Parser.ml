type parser_state = {
  remaining : BatSubstring.t;
      (** Text still to be parsed. Generally a suffix of the original input. We
          use a BatSubstring for O(1) slicing. *)
  loc : Location.t;
      (** Current location of the parser relative to the source text. *)
}
(** Type representing an immutable parser state. *)

type ('a, 'e) parser = parser_state -> ('a * parser_state, 'e) result
(** The parser type. A function taking the current parser state and returning
    either an output and a new state, or an error. *)

(** [l @> r] composes its right argument after its left argument.

    Equivalent to Fun.compose r l.

    @param l a function.
    @param r a function.
    @return the composition of r after l. *)
let ( @> ) : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c =
 fun l r -> Fun.compose r l

(** [l @>> r] is the composition of r after l under the Result monad.

    This is a building block used to implement sequential composition of
    parsers. Corresponds to the following parsing expression:

    l r *)
let ( @>> ) :
    'a 'b 'c 'e.
    ('a -> ('b, 'e) result) -> ('b -> ('c, 'e) result) -> 'a -> ('c, 'e) result
    =
 fun prefix suffix state -> Result.bind (prefix state) suffix

(** [replace f] lifts f so it can apply to the output of a parser.

    In categorical terms, this function together with type ('a, 'e) parser form
    a functor.

    Because the grammar does not specify what to do with the results of parsing,
    this function does not correspond to anything in the grammar. *)
let replace :
    'a 's 'b 'e. ('a -> 'b) -> ('a * 's, 'e) result -> ('b * 's, 'e) result =
 fun f -> Result.map (fun (a, s) -> (f a, s))

(** [ifte test consequent alternative] constructs a parser that attempts to
    parse test. On success, the parser commits to parsing consequent afterwards.
    On failure, the parser backtracks and tries alternative instead.

    Corresponds to the following parsing expression:

    test consequent / !test alternative *)
let ifte :
    'a 'b 'c 'et 'ec.
    ('a -> ('b, 'et) result) ->
    ('b -> ('c, 'ec) result) ->
    ('a -> ('c, 'ec) result) ->
    'a ->
    ('c, 'ec) result =
 fun test consequent alternative state ->
  state |> test
  |> Result.fold ~ok:consequent ~error:(fun _ -> alternative state)

(** [l @|| r] constructs a parser that attempts to parse l. On failure, the
    parser backtracks and tries r instead.

    Corresponds to the following parsing expression:

    l / r

    Note that if l is itself a sequential composition and r starts by negating a
    prefix of l, ifte is more efficient. That is, instead of doing this:

    (a \@&& b) \@|| (is_not a \@&& c)

    You should prefer this instead:

    ifte a b c *)
let ( @|| ) l r = ifte l Result.ok r

(** [l @&& r] constructs a parser that attempts to parse l followed by r,
    discarding the output of l.

    Corresponds to the following parsing expression:

    l r *)
let ( @&& ) :
    ('a -> ('b * 'c, 'd) result) ->
    ('c -> ('e, 'd) result) ->
    'a ->
    ('e, 'd) result =
 fun l r -> l @> Result.map snd @>> r

(** [return output] constructs a parser that succeeds without consuming any
    input and outputs its argument.

    Corresponds to the following parsing expression:

    "" *)
let return : 'a 'e. 'a -> ('a, 'e) parser =
 fun output state -> Ok (output, state)

(** A parser succeeds without consuming any input and outputs unit.

    Corresponds to the following parsing expression:

    "" *)
let succeed : 'e. (unit, 'e) parser = return ()

(** [is_not p handler] constructs a parser that attempts to parse p. On success,
    the parser then fails (using handler to construct the error value). On
    failure, the parser backtracks and succeeds without consuming input.

    Corresponds to the following parsing expression:

    !p *)
let is_not :
    'a 'errin 'errout.
    ('a, 'errin) parser ->
    ('a -> Location.location -> 'errout) ->
    (unit, 'errout) parser =
 fun p handler ({ loc = startl; _ } as state) ->
  state
  |> ifte p
       (fun (r, { loc = endl; _ }) -> Error (handler r { startl; endl }))
       succeed

(** [is p] constructs a parser that attempts to parse p. On success, the parser
    then backtracks and succeeds without consuming input. On failure, the parser
    fails.

    Corresponds to the following parsing expression:

    &p *)
let is : 'a 'e. ('a, 'e) parser -> ('a, 'e) parser =
 fun p state -> state |> p @>> fun (r, _) -> Ok (r, state)

(** [capture f] lifts a function returning a parser so it can apply to a
    successful parser result.

    Because the grammar does not specify what to do with the results of parsing,
    this function does not correspond to anything in the grammar. *)
let capture :
    'a 'b 'e.
    ('a -> ('b, 'e) parser) ->
    'a * parser_state ->
    ('b * parser_state, 'e) result =
 fun f (result, state) -> f result state

(** [maybe p] constructs a parser that attempts to parse p. On success, the
    parser succeeds. On failure, the parser backtracks and succeeds.

    Corresponds to the following parsing expression:

    p? *)
let maybe : 'a 'el 'er. ('a, 'el) parser -> ('a option, 'er) parser =
 fun p -> ifte p (capture @@ Option.some @> return) (return None)

(** [ignoring p] augments a parser to capture the output of a previous parser,
    continue parsing using the state, discard its output and replace it with the
    captured output.

    Because the grammar does not specify what to do with the results of parsing,
    this corresponds to the following parsing expression:

    p *)
let ignoring :
    'a 'b 'e.
    ('a, 'e) parser -> 'b * parser_state -> ('b * parser_state, 'e) result =
 fun p -> capture @@ fun result -> p @&& return result

(** [star p] constructs a parser that attempts to parse p repeatedly for as long
    as it succeeds, accumulating its outputs into a BatFingerTree.

    Corresponds to the following parsing expression:

    p* *)
let star : 'a 'e 'none. ('a, 'e) parser -> ('a FT.t, 'none) parser =
 fun p ->
  let rec loop acc =
    ifte p (capture @@ fun elem -> loop (FT.snoc acc elem)) (return acc)
  in
  loop FT.empty

(** [plus p] constructs a parser that attempts to parse p once, then attempts to
    parse p repeatedly for as long as it succeeds, accumulating its outputs into
    a BatFingerTree.

    Corresponds to the following parsing expression:

    p+

    Which is equivalent to the following parsing expression:

    p p* *)
let plus p =
  p @>> capture @@ fun first -> star p @> replace @@ Fun.flip FT.cons first

open BatSubstring

(** [stride previous current] computes the difference between the substrings
    previous and current.

    Since we always call this using a suffix of the first argument as the second
    argument, for our purposes this is equivalent to computing how many
    characters the parser has advanced. *)
let stride previous current = size previous - size current

(** [some] is a parser that attempts to parse any single character. It fails
    only when there are no more characters to parse.

    Corresponds to the following parsing expression:

    . *)
let some : 'e. (unit, ([> `UnexpectedEOF ] as 'e)) parser = function
  | { remaining; loc } -> (
      match first remaining with
      | None -> Error `UnexpectedEOF
      | Some _ ->
          Ok ((), { remaining = triml 1 remaining; loc = Location.step 1 loc }))

(** Grammar rule:

    horizontal_whitespace <- (' ' / '\t')+ *)
let horizontal_whitespace :
    'e. (unit, ([> `ExpectedHorizontalWhitespace of Location.t ] as 'e)) parser
    =
 fun { remaining = current; loc } ->
  let remaining = dropl (function ' ' | '\t' -> true | _ -> false) current in
  match stride current remaining with
  | 0 -> Error (`ExpectedHorizontalWhitespace loc)
  | dropped -> Ok ((), { remaining; loc = Location.step dropped loc })

(** [literal l] constructs a parser that matches l with the prefix of the
    remaining text.

    Corresponds to terminal parsing expressions. *)
let literal l { remaining; loc } =
  if BatSubstring.is_prefix l remaining then
    Ok
      ( (),
        {
          remaining = triml (String.length l) remaining;
          loc = Location.plus_str l loc;
        } )
  else Error (`WrongPrefix (loc, l))

(** Grammar rule:

    newline <- '\n' / '\r\n' *)
let newline = literal "\n" @|| literal "\r\n"

(** Grammar rule:

    colon <- ':' *)
let colon = literal ":"

(** Grammar rule:

    percent <- '%' *)
let percent = literal "%"

(** Grammar rule:

    hash <- '#' *)
let hash = literal "#"

(** Grammar rule:

    single_quote <- "'" *)
let single_quote = literal "'"

(** Grammar rule:

    comma <- ',' *)
let comma = literal ","

(** Grammar rule:

    period <- '.' *)
let period = literal "."

(** Grammar rule:

    pipe <- '|' *)
let pipe = literal "|"

(** Grammar rule:

    question_mark <- '?' *)
let question_mark = literal "?"

(** Grammar rule:

    left_paren <- '(' *)
let left_paren = literal "("

(** Grammar rule:

    right_paren <- ')' *)
let right_paren = literal ")"

(** Grammar rule:

    left_bracket <- '\[' *)
let left_bracket = literal "["

(** Grammar rule:

    right_bracket <- '\]' *)
let right_bracket = literal "]"

(** Grammar rule:

    left_curly_brace <- '\{' *)
let left_curly_brace = literal "{"

(** Grammar rule:

    right_curly_brace <- '\}' *)
let right_curly_brace = literal "}"

(** Grammar rule:

    holds <- ':-' *)
let holds = literal ":-"

(** Grammar rule:

    line <- newline / (. line / !.) *)
let rec line : 'e. (unit, 'e) parser =
 fun state -> state |> newline @|| ifte some (snd @> line) succeed

(** Grammar rule:

    whitespace <- horizontal_whitespace? (newline whitespace / !newline) *)
let rec whitespace : 'e. (unit, 'e) parser =
 fun state ->
  state
  |> maybe horizontal_whitespace
     @>> snd
     @> ifte newline (snd @> whitespace) succeed

(** [ident_like is_start is_character fallthrough] constructs a parser that
    attempts to parse a sequence of characters where the first character
    satisfies the predicate [is_start], while every other character satisfies
    [is_character]. *)
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

(** Grammar rule:

    atom <- \[a-z\] (\[a-zA-Z0-9\] / '_' / '-')* *)
let atom :
    'e.
    ( string Location.with_location,
      ([> `UnexpectedEOF | `ExpectedLowercase of Location.t ] as 'e) )
    parser =
  ident_like BatChar.is_lowercase
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_' || c = '-')
    (fun loc -> `ExpectedLowercase loc)

(** Grammar rule:

    variable <- ('_' / \[A-Z\]) (\[a-zA-Z0-9\] / '_')* *)
let variable :
    'e.
    ( string Location.with_location,
      ([> `ExpectedUppercaseOrUnderscore of Location.t | `UnexpectedEOF ] as 'e)
    )
    parser =
  ident_like
    (function
      | '_' -> true | c when BatChar.is_uppercase c -> true | _ -> false)
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_')
    (fun loc -> `ExpectedUppercaseOrUnderscore loc)

(** Grammar rule:

    quoted_atom <- '\'' (!'\'' !'\n' .)* '\'' *)
let quoted_atom :
    'e.
    ( string Location.with_location,
      ([> `UnexpectedEOF | `WrongPrefix of Location.t * string ] as 'e) )
    parser =
 fun { remaining = current; loc } ->
  match getc current with
  | Some ('\'', remaining) ->
      let atom_name, remaining =
        splitl (function '\'' | '\n' -> false | _ -> true) remaining
      in
      let next_loc = Location.step (stride current remaining) loc in
      { remaining; loc = next_loc }
      |> single_quote @>> fun ((), ({ loc = endl; _ } as state)) ->
         Ok
           (Location.add_loc (to_string atom_name) { startl = loc; endl }, state)
  | Some _ | None -> Error (`WrongPrefix (loc, "\'"))

(** Grammar rule:

    integer <- '-'? \[0-9\]+ *)
let integer :
    'e.
    ( int Location.with_location,
      ([> `NotADigit of Location.t | `UnexpectedEOF ] as 'e) )
    parser =
 fun ({ loc = startl; _ } as state) ->
  let positive_integer { remaining = current; loc = after_minus } =
    match size current with
    | 0 -> Error `UnexpectedEOF
    | _ -> (
        let digits, remaining = splitl BatChar.is_digit current in
        let endl = Location.step (stride current remaining) after_minus in
        match to_string digits with
        | "" -> Error (`NotADigit after_minus)
        | digits ->
            Ok
              ( Location.add_loc (int_of_string digits) { startl; endl },
                { remaining; loc = endl } ))
  in
  state
  |> ifte (literal "-")
       (snd @> positive_integer @> replace @@ Location.fmap Int.neg)
       positive_integer

(** [list_of ~start_delim ~separator ~end_delim item] constructs a parser with
    the following structure:

    items <- start_delim (end_delim / !end_delim item item_loop)

    item_loop <- separator item item_loop / !(separator item) end_delim *)
let list_of :
    'a 'e.
    start_delim:(unit, 'e) parser ->
    separator:(unit, 'e) parser ->
    end_delim:(unit, 'e) parser ->
    ('a, 'e) parser ->
    ('a FT.t, 'e) parser =
 fun ~start_delim ~separator ~end_delim item ->
  let open FT in
  start_delim
  @&& ifte end_delim (snd @> return empty)
  @@ item @> replace singleton
  @>>
  let rec loop acc =
    ifte
      (separator @&& item @> replace (snoc acc))
      (capture loop)
      (end_delim @&& return acc)
  in
  capture loop

(** Grammar rule:

    func_label <- (atom colon)* (quoted_atom / atom) *)
let func_label :
    'e.
    ( Ast.Expr.func_label Location.with_location,
      ([> `UnexpectedEOF
       | `ExpectedLowercase of Location.t
       | `WrongPrefix of Location.t * string ]
       as
       'e) )
    parser =
 fun ({ loc = startl; _ } as state) ->
  state
  |> star (atom @>> ignoring colon)
     @>> capture
     @@ fun qualifiers ->
     (quoted_atom @|| atom)
     @>> fun (label_name, ({ loc = endl; _ } as state)) ->
     Ok (Location.add_loc (qualifiers, label_name) { startl; endl }, state)

type expr_errors =
  [ `ExpectedLowercase of Location.t
  | `UnexpectedEOF
  | `WrongPrefix of Location.t * string ]

(** Grammar rule:

    expr <- whitespace_and_comments (integer / !integer (variable / !variable
    (func / !func list))) *)
let rec expr : 'e. (Ast.Expr.t, ([> expr_errors ] as 'e)) parser =
 fun state ->
  state
  |> whitespace_and_comments
     @&& ifte integer (capture @@ Location.fmap Ast.Expr.integer @> return)
     @@ ifte variable (capture @@ Location.fmap Ast.Expr.variable @> return)
     @@ ifte func (capture @@ Location.fmap Ast.Expr.functorr @> return)
     @@ list

(** Grammar rule:

    whitespace_and_comments <- whitespace (percent line whitespace_and_comments
    / !percent (hash percent expr whitespace_and_comments / !(hash percent))) *)
and whitespace_and_comments : 'e. (unit, ([> expr_errors ] as 'e)) parser =
 fun state ->
  state
  |> whitespace
     @&& ifte percent (snd @> line @&& whitespace_and_comments)
     @@ ifte (hash @&& percent)
          (snd @> expr @&& whitespace_and_comments)
          succeed

(** Grammar rules:

    list <- left_bracket whitespace_and_comments (right_bracket / !right_bracket
    expr whitespace_and_comments list_tail)

    list_tail <- comma whitespace_and_comments expr whitespace_and_comments
    list_tail / !comma (pipe whitespace_and_comments expr)?
    whitespace_and_comments right_bracket *)
and list : 'e. (Ast.Expr.t, ([> expr_errors ] as 'e)) parser =
 fun ({ loc = startl; _ } as state) ->
  let build_cons startl endl prefix (tail : Ast.Expr.t) : Ast.Expr.t =
    FT.fold_right
      (fun acc elem ->
        Location.add_loc
          (Ast.Expr.Cons (elem, acc))
          { startl = elem.loc.startl; endl })
      tail prefix
    |> fun { loc; content } ->
    { Location.content; loc = { startl; endl = loc.endl } }
  in
  let nil startl endl =
    { Location.content = Ast.Expr.Nil; loc = { Location.startl; endl } }
  in
  let rec list_tail acc =
    ifte comma
      (snd @> whitespace_and_comments @&& expr @>> capture
      @@ fun elem -> whitespace_and_comments @&& list_tail @@ FT.snoc acc elem)
    @@ maybe (pipe @&& whitespace_and_comments @&& expr)
    @>> ignoring (whitespace_and_comments @&& right_bracket)
    @>> fun (tail_expr, ({ loc = endl; _ } as state)) ->
    Ok
      ( tail_expr
        |> Option.fold ~none:(nil startl endl) ~some:Fun.id
        |> build_cons startl endl acc,
        state )
  in
  state
  |> left_bracket @&& whitespace_and_comments
     @&& ifte right_bracket
           ( snd @> fun ({ loc = endl; _ } as state) ->
             Ok (nil startl endl, state) )
     @@ expr @> replace FT.singleton
     @>> ignoring whitespace_and_comments
     @>> capture list_tail

(** Grammar rules:

    func <- func_label ((&left_bracket / &left_paren) (prolog_elements /
    karuta_elements) / !(&left_bracket / &left_paren))

    single_element <- whitespace_and_comments expr

    karuta_elements <- left_bracket (whitespace_and_comments right_bracket /
    !(whitespace_and_comments right_bracket) single_element
    karuta_elements_loop)

    karuta_elements_loop <- whitespace_and_comments comma expr
    karuta_elements_loop / !(whitespace_and_comments comma expr)
    whitespace_and_comments right_bracket

    prolog_elements <- left_paren (whitespace_and_comments right_paren /
    !(whitespace_and_comments right_paren) single_element prolog_elements_loop)

    prolog_elements_loop <- whitespace_and_comments comma expr
    prolog_elements_loop / !(whitespace_and_comments comma expr)
    whitespace_and_comments right_paren *)
and func :
    'e. (Ast.Expr.func Location.with_location, ([> expr_errors ] as 'e)) parser
    =
 fun ({ loc = startl; _ } as state) ->
  let single_element = whitespace_and_comments @&& expr in
  let karuta_elements =
    list_of ~start_delim:left_bracket
      ~separator:(whitespace_and_comments @&& comma)
      ~end_delim:(whitespace_and_comments @&& right_bracket)
    @@ single_element
  in
  let prolog_elements =
    list_of ~start_delim:left_paren
      ~separator:(whitespace_and_comments @&& comma)
      ~end_delim:(whitespace_and_comments @&& right_paren)
    @@ single_element
  in
  state
  |> func_label @>> capture
     @@ fun label ->
     ifte
       (is left_bracket @|| is left_paren)
       (snd
       @> (prolog_elements @|| karuta_elements)
       @>> fun (args, ({ loc = endl; _ } as state)) ->
       Ok
         ( Location.add_loc
             (Ast.Expr.func Location.(label.content) args)
             { startl; endl },
           state ))
     @@ return (Location.fmap Ast.Expr.atom label)

(* TODO: for the composites below, the location around the result should encompass
   the entire text, not just the first element *)

(** Grammar rule:

    query <- question_mark / !question_mark whitespace_and_comments comma
    whitespace_and_comments func whitespace_and_comments query *)
let query :
    'e.
    Ast.Expr.func Location.with_location ->
    ( Ast.Expr.func Location.with_location FT.t Location.with_location,
      ([> expr_errors ] as 'e) )
    parser =
 fun first_element ->
  let rec query_loop elements =
    ifte question_mark (snd @> return elements)
    @@ whitespace_and_comments @&& comma @&& whitespace_and_comments @&& func
    @>> ignoring whitespace_and_comments
    @> replace (FT.snoc elements)
    @>> capture query_loop
  in
  query_loop (FT.singleton first_element)
  @> replace
  @@ fun l -> Location.add_loc l first_element.loc

(** Grammar rules:

    declaration <- (holds whitespace_and_comments declaration_body)?
    whitespace_and_comments period

    declaration_body <- func whitespace_and_comments (comma
    whitespace_and_comments declaration_body / !comma) *)
let declaration :
    'e.
    Ast.Expr.func Location.with_location ->
    ( Ast.ParserClause.decl Location.with_location,
      ([> expr_errors ] as 'e) )
    parser =
 fun { Location.content = head; loc } ->
  let rec body_loop elements =
    func @>> capture
    @@ fun elem ->
    whitespace_and_comments
    @&& ifte comma
          (snd @> whitespace_and_comments @&& body_loop @@ FT.snoc elements elem)
    @@ return @@ FT.snoc elements elem
  in
  maybe (holds @&& whitespace @&& body_loop FT.empty)
  @>> ignoring (whitespace_and_comments @&& period)
  @> replace
       ( Option.fold ~none:FT.empty ~some:Fun.id @> fun body ->
         Location.add_loc { Ast.ParserClause.head; body } loc )

(** Grammar rule:

    clause <- holds whitespace_and_comments directive / !holds func
    whitespace_and_comments (&(question_mark / comma) query / !(question_mark /
    comma) declaration) whitespace_and_comments *)
let rec parser_clause :
    'e. (Ast.ParserClause.t, ([> expr_errors ] as 'e)) parser =
 fun state ->
  state
  |> (ifte holds
        (snd @> whitespace_and_comments @&& directive @> replace
        @@ Location.fmap Ast.ParserClause.directive)
     @@ func @>> capture
     @@ fun first ->
     whitespace_and_comments
     @&& ifte
           (is @@ question_mark @|| comma)
           (snd @> query first @> replace (Location.fmap Ast.ParserClause.query))
           (declaration first @> replace
           @@ Location.fmap Ast.ParserClause.declaration))
     @>> ignoring whitespace_and_comments

(** Grammar rule:

    directive <- func (whitespace_and_comments left_curly_brace top_level
    right_curly_brace whitespace_and_comments)* period whitespace_and_comments
*)
and directive :
    'e.
    ( (Ast.Expr.func Location.with_location
      * Ast.ParserClause.t FT.t Location.with_location FT.t)
      Location.with_location,
      ([> expr_errors ] as 'e) )
    parser =
 fun state ->
  state
  |> func @>> capture
     @@ fun header ->
     star
       (whitespace_and_comments @&& left_curly_brace @&& top_level @>> ignoring
      @@ right_curly_brace @&& whitespace_and_comments)
     @>> ignoring (period @&& whitespace_and_comments)
     @> replace
     @@ fun bodies -> Location.add_loc (header, bodies) Location.(header.loc)

(** Grammar rule:

    top_level <- whitespace_and_comments (clause whitespace_and_comments)* *)
and top_level :
    'e.
    ( Ast.ParserClause.t FT.t Location.with_location,
      ([> expr_errors ] as 'e) )
    parser =
 fun ({ loc = startl; _ } as state) ->
  state
  |> whitespace_and_comments
     @&& star (parser_clause @>> ignoring whitespace_and_comments)
     @>> fun (result, ({ loc = endl; _ } as state)) ->
     Ok (Location.add_loc result { startl; endl }, state)

(** Grammar rule:

    start <- top_level !. *)
let parse (filepath : string) (source : string) =
  match
    {
      remaining = BatSubstring.all source;
      loc = { pos_fname = filepath; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };
    }
    |> top_level @>> capture
       @@ fun file ->
       is_not some (fun () loc -> `ExpectedEOF (file, loc)) @&& return file
  with
  | Ok (parsed, _) -> parsed.content
  | Error e ->
      (match e with
      | `ExpectedEOF (_, loc) ->
          Logger.error loc "Expected the file to end, but it continued"
      | `ExpectedLowercase loc ->
          Logger.error (Location.double loc)
            "Expected a lower case letter, but got something else"
      | `UnexpectedEOF ->
          Logger.simply_error "File ended, but we expected it to continue"
      | `WrongPrefix (loc, expected_prefix) ->
          Logger.error
            { startl = loc; endl = Location.plus_str expected_prefix loc }
          @@ "We were expecting a '"
          ^ String.escaped expected_prefix
          ^ "', but got this instead.");
      exit 1
