type parser_state = {
  remaining : BatSubstring.t;
      (** Text still to be parsed. Generally a suffix of the original input. We
          use a BatSubstring for O(1) slicing. *)
  loc : Location.point;
      (** Current location of the parser relative to the source text. *)
}
(** Type representing an immutable parser state. *)

type parser_error = {
  region : Location.region;
  expected : string;
  gotten : string;
}

type 'a parser = parser_state -> ('a * parser_state, parser_error) result
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
let ( @>> ) : 'a 'b 'e. 'a parser -> ('a -> 'b parser) -> 'b parser =
 fun prefix f state ->
  Result.bind (prefix state) (fun (a, state') -> f a state')

let parser_map : 'a 'b 'e. ('a -> 'b) -> 'a parser -> 'b parser =
 fun f p state -> p state |> Result.map (fun (a, state') -> (f a, state'))

let parser_map_error : (parser_error -> parser_error) -> 'a parser -> 'a parser
    =
 fun f p state -> p state |> Result.map_error f

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
let ifte : 'a parser -> ('a -> 'b parser) -> 'b parser -> 'b parser =
 fun test consequent alternative state ->
  state |> test
  |> Result.fold
       ~ok:(fun (a, state') -> consequent a state')
       ~error:(fun _ -> alternative state)

(** [return output] constructs a parser that succeeds without consuming any
    input and outputs its argument.

    Corresponds to the following parsing expression:

    "" *)
let return : 'a -> 'a parser = fun output state -> Ok (output, state)

(** [l @|| r] constructs a parser that attempts to parse l. On failure, the
    parser backtracks and tries r instead.

    Corresponds to the following parsing expression:

    l / r

    Note that if l is itself a sequential composition and r starts by negating a
    prefix of l, ifte is more efficient. That is, instead of doing this:

    (a \@&& b) \@|| (is_not a \@&& c)

    You should prefer this instead:

    ifte a b c *)
let ( @|| ) l r = ifte l return r

(** [l @&& r] constructs a parser that attempts to parse l followed by r,
    discarding the output of l.

    Corresponds to the following parsing expression:

    l r *)
let ( @&& ) : 'a parser -> 'b parser -> 'b parser = fun l r -> l @>> fun _ -> r

(** [l @&&& r] constructs a parser that attempts to parse l followed by r,
    discarding the output of r.

    Corresponds to the following parsing expression:

    l r *)
let ( @&&& ) : 'a parser -> 'b parser -> 'a parser =
 fun l r -> l @>> fun a -> r @&& return a

(** A parser succeeds without consuming any input and outputs unit.

    Corresponds to the following parsing expression:

    "" *)
let succeed : unit parser = return ()

(** [is_not p] constructs a parser that attempts to parse p. On success, the
    parser then fails. On failure, the parser backtracks and succeeds without
    consuming input.

    Corresponds to the following parsing expression:

    !p *)
let is_not : 'a parser -> unit parser =
 (* TODO: how to deal with dummy errors *)
 fun p state ->
  match p state with
  | Ok r -> Error { gotten = ""; expected = ""; region = Location.dummy }
  | Error _ -> Ok ((), state)

(** [is p] constructs a parser that attempts to parse p. On success, the parser
    then backtracks and succeeds without consuming input. On failure, the parser
    fails.

    Corresponds to the following parsing expression:

    &p *)
let is : 'a 'e. 'a parser -> 'a parser =
 (* fun p state -> state |> p @>> fun (r, _) -> Ok (r, state) *)
 fun p state ->
  p state |> Result.map (fun (a, _) -> (a, state))

(** [maybe p] constructs a parser that attempts to parse p. On success, the
    parser succeeds. On failure, the parser backtracks and succeeds.

    Corresponds to the following parsing expression:

    p? *)
let maybe : 'a parser -> 'a option parser =
 fun p -> ifte p (Option.some @> return) (return None)

(** [star p] constructs a parser that attempts to parse p repeatedly for as long
    as it succeeds, accumulating its outputs into a BatFingerTree.

    Corresponds to the following parsing expression:

    p* *)
let star : 'a parser -> 'a FT.t parser =
 fun p ->
  let rec loop acc =
    ifte p (fun elem -> loop (FT.snoc acc elem)) (return acc)
  in
  loop FT.empty

(** [plus p] constructs a parser that attempts to parse p once, then attempts to
    parse p repeatedly for as long as it succeeds, accumulating its outputs into
    a BatFingerTree.

    Corresponds to the following parsing expression:

    p+

    Which is equivalent to the following parsing expression:

    p p* *)
let plus p = p @>> fun first -> star p @> replace @@ Fun.flip FT.cons first

open BatSubstring

(** [stride previous current] computes the difference between the substrings
    previous and current.

    Since we always call this using a suffix of the first argument as the second
    argument, for our purposes this is equivalent to computing how many
    characters the parser has advanced. *)
let stride previous current = size previous - size current

(* TODO: improver parser Ok payload and error *)
let char_predicate f error_message = function
  | { remaining; loc } -> (
      match first remaining with
      | Some c when f c ->
          Ok
            ( (),
              {
                remaining = triml 1 remaining;
                loc = { loc with coordinate = Location.step 1 loc.coordinate };
              } )
      | Some _ | None ->
          Error
            {
              gotten =
                Printf.sprintf "`%s`"
                  (BatSubstring.first remaining
                  |> Option.fold ~none:"EOF" ~some:(String.make 1));
              expected = error_message;
              region = Location.double loc.filename loc.coordinate;
            })

(** [some] is a parser that attempts to parse any single character. It fails
    only when there are no more characters to parse.

    Corresponds to the following parsing expression:

    . *)
let some = char_predicate (fun _ -> true) "a character"

(** Grammar rule:

    horizontal_whitespace <- (' ' / '\t')+ *)
let horizontal_whitespace : unit parser =
 fun { remaining = current; loc } ->
  let remaining = dropl (function ' ' | '\t' -> true | _ -> false) current in
  match stride current remaining with
  | 0 ->
      Error
        {
          gotten =
            Printf.sprintf "`%s`"
              (BatSubstring.first current
              |> Option.fold ~none:"EOF" ~some:(String.make 1));
          (* TODO: improve error message *)
          expected = "` ` or `\t`";
          region = Location.double loc.filename loc.coordinate;
        }
  | dropped ->
      Ok
        ( (),
          {
            remaining;
            loc = { loc with coordinate = Location.step dropped loc.coordinate };
          } )

(** [literal l] constructs a parser that matches l with the prefix of the
    remaining text.

    Corresponds to terminal parsing expressions. *)
let literal l { remaining; loc } =
  if BatSubstring.is_prefix l remaining then
    Ok
      ( (),
        {
          remaining = triml (String.length l) remaining;
          loc = { loc with coordinate = Location.plus_str l loc.coordinate };
        } )
  else
    Error
      {
        gotten =
          Printf.sprintf "`%s`"
            (BatSubstring.first remaining
            |> Option.fold ~none:"EOF" ~some:(String.make 1));
        (* TODO: improve error message *)
        expected = Printf.sprintf "`%s`" l;
        region = Location.double loc.filename loc.coordinate;
      }

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
let rec line : unit parser =
 fun state -> state |> newline @|| ifte some (fun _ -> line) succeed

(** Grammar rule:

    whitespace <- horizontal_whitespace? (newline whitespace / !newline) *)
let rec whitespace : unit parser =
 fun state ->
  state
  |> maybe horizontal_whitespace @&& ifte newline (fun _ -> whitespace) succeed

(* TODO: maybe adding a note that for any character that satisfies `is_character` it must also satisfy `is_start` *)

(** [ident_like is_start is_character fallthrough] constructs a parser that
    attempts to parse a sequence of characters where the first character
    satisfies the predicate [is_start], while every other character satisfies
    [is_character]. *)
let ident_like is_start is_character fallthrough { remaining = current; loc } =
  match first current with
  | None -> Error `UnexpectedEOF
  | Some c when is_start c ->
      let atom, remaining = splitl is_character current in
      let next_loc =
        {
          loc with
          coordinate = Location.step (stride current remaining) loc.coordinate;
        }
      in
      Ok
        ( Location.delimit loc next_loc (to_string atom),
          { remaining; loc = next_loc } )
  | Some _ -> Error (fallthrough loc)

(** Grammar rule:

    atom <- \[a-z\] (\[a-zA-Z0-9\] / '_' / '-')* *)
let atom : string Location.with_location parser =
  ident_like BatChar.is_lowercase
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_' || c = '-')
    (fun loc -> `ExpectedLowercase loc)

(** Grammar rule:

    variable <- '_'? \[A-Z\] (\[a-zA-Z0-9\] / '_')* *)
let variable : string Location.with_location parser =
  (* _ *)
  (* _A *)
  (* _AasdasdaASd123 *)
  (* fun { remaining; loc } ->
  failwith "" *)
  ident_like
    (function
      | '_' -> true | c when BatChar.is_uppercase c -> true | _ -> false)
    (fun c -> BatChar.is_letter c || BatChar.is_digit c || c = '_')
    (fun loc -> `ExpectedUppercaseOrUnderscore loc)

(** Grammar rule:

    quoted_atom <- "'" (!"'" !'\n' .)* "'" *)
let quoted_atom : 'e. string Location.with_location parser =
 fun { remaining = current; loc } ->
  match getc current with
  | Some ('\'', remaining) ->
      let atom_name, remaining =
        splitl (function '\'' | '\n' -> false | _ -> true) remaining
      in
      let next_loc =
        {
          loc with
          coordinate = Location.step (stride current remaining) loc.coordinate;
        }
      in
      { remaining; loc = next_loc }
      |> single_quote @>> fun () ->
         fun ({ loc = endl; _ } as state) ->
          Ok (Location.delimit loc endl (to_string atom_name), state)
  | Some _ | None -> Error (`WrongPrefix (loc, "\'"))

(** Grammar rule:

    integer <- '-'? \[0-9\]+ *)
let integer : 'e. int Location.with_location parser =
 fun ({ loc = startl; _ } as state) ->
  let positive_integer { remaining = current; loc = after_minus } =
    match size current with
    | 0 -> Error `UnexpectedEOF
    | _ -> (
        let digits, remaining = splitl BatChar.is_digit current in
        let endl =
          {
            after_minus with
            coordinate =
              Location.step (stride current remaining) after_minus.coordinate;
          }
        in
        match to_string digits with
        | "" -> Error (`NotADigit after_minus)
        | digits ->
            Ok
              ( Location.delimit startl endl (int_of_string digits),
                { remaining; loc = endl } ))
  in
  state
  |> ifte (literal "-")
       (fun _ -> positive_integer @> replace @@ Location.fmap Int.neg)
       positive_integer

(** [list_of ~start_delim ~separator ~end_delim item] constructs a parser with
    the following structure:

    items <- start_delim (end_delim / !end_delim item item_loop)

    item_loop <- separator item item_loop / !(separator item) end_delim *)
let list_of :
    start_delim:unit parser ->
    separator:unit parser ->
    end_delim:unit parser ->
    'a parser ->
    'a FT.t parser =
 fun ~start_delim ~separator ~end_delim item ->
  let open FT in
  let rec loop acc =
    ifte
      (separator @&& item @> replace (snoc acc))
      loop
      (end_delim @&& return acc)
  in
  start_delim
  @&& ifte end_delim (fun _ -> return empty)
  @@ item
  @>> fun item -> loop (singleton item)

(** Grammar rule:

    func_label <- (atom colon)* (quoted_atom / atom) *)
let func_label : 'e. Ast.Expr.func_label Location.with_location parser =
 fun ({ loc = startl; _ } as state) ->
  state
  |> star (atom @&&& colon) @>> fun qualifiers ->
     (quoted_atom @|| atom) @>> fun label_name ->
     fun ({ loc = endl; _ } as state) ->
      Ok (Location.delimit startl endl (qualifiers, label_name), state)

let is_integer_start c = c = '-' || BatChar.is_digit c
let is_variable_start c = c = '_' || BatChar.is_uppercase c
let is_func_start c = c = '\'' || BatChar.is_lowercase c
let is_list_start c = c = '['

let expr_start =
  char_predicate
    (fun c ->
      is_integer_start c || is_variable_start c || is_func_start c
      || is_list_start c)
    "expression" (* TODO: expression is not accurate *)

(** Grammar rule:

    expr <- integer / variable / func / list *)
let rec expr =
 fun state ->
  state
  |> ifte
       (is @@ char_predicate is_integer_start "unreachable")
       (fun _ -> integer |> parser_map @@ Location.fmap Ast.Expr.integer)
     @@ ifte
          (is @@ char_predicate is_variable_start "unreachable")
          (fun _ -> variable |> parser_map @@ Location.fmap Ast.Expr.variable)
     @@ ifte
          (is @@ char_predicate is_func_start "unreachable")
          (fun _ -> func |> parser_map @@ Location.fmap Ast.Expr.functorr)
     @@ ifte (is @@ char_predicate is_list_start "unreachable") (fun _ -> list)
     @@ fun state ->
     Error
       {
         gotten =
           Printf.sprintf "`%s`"
             (BatSubstring.first state.remaining
             |> Option.fold ~none:"EOF" ~some:(String.make 1));
         (* TODO: improve error message *)
         expected = "expression";
         region = Location.double state.loc.filename state.loc.coordinate;
       }

(** Grammar rule:

    whitespace_and_comments <- whitespace (percent line whitespace_and_comments
    / !percent (hash percent expr whitespace_and_comments / !(hash percent))) *)
and whitespace_and_comments : unit parser =
 fun state ->
  state
  |> whitespace
     @&& ifte percent (fun _ -> line @&& whitespace_and_comments)
     @@ ifte (hash @&& percent)
          (fun _ -> expr @&& whitespace_and_comments)
          succeed

(** Grammar rules:

    list <- left_bracket whitespace_and_comments (right_bracket / expr
    whitespace_and_comments list_tail)

    list_tail <- right_bracket / pipe whitespace_and_comments expr
    whitespace_and_comments right_bracket / comma whitespace_and_comments expr
    whitespace_and_comments list_tail *)
and list : 'e. Ast.Expr.t parser =
 (* DONE *)
 fun ({ loc = startl; _ } as state) ->
  let build_cons (startl : Location.point) (endl : Location.point)
      (prefix : Ast.Expr.t FT.t) (tail : Ast.Expr.t) : Ast.Expr.t =
    FT.fold_right
      (fun acc (elem : Ast.Expr.t) ->
        Location.delimit
          { filename = startl.filename; coordinate = elem.loc.startl }
          endl
          (Ast.Expr.Cons (elem, acc)))
      tail prefix
    |> fun { loc; content } ->
    {
      Location.content;
      loc = { loc with startl = startl.coordinate; endl = loc.endl };
    }
  in
  let nil startl endl = Location.delimit startl endl Ast.Expr.Nil in
  let rec list_tail acc =
    ifte right_bracket (fun _ ->
        fun ({ loc = endl; _ } as state) ->
         Ok (nil startl endl |> build_cons startl endl acc, state))
    @@ ifte pipe (fun _ ->
        (whitespace_and_comments @&& expr @&&& whitespace_and_comments
       @&&& right_bracket)
        @>> fun tail_expr ->
        fun ({ loc = endl; _ } as state) ->
         Ok (build_cons startl endl acc tail_expr, state))
    @@ ifte comma
         (fun _ ->
           (whitespace_and_comments @&& expr @&&& whitespace_and_comments)
           @>> fun element -> list_tail @@ FT.snoc acc element)
         (fun state ->
           Error
             {
               gotten =
                 Printf.sprintf "`%s`"
                   (BatSubstring.first state.remaining
                   |> Option.fold ~none:"EOF" ~some:(String.make 1));
               (* TODO: improve error message *)
               expected = "`]` or `|` or `,`";
               region = Location.double state.loc.filename state.loc.coordinate;
             })
  in
  state
  |> left_bracket @&& whitespace_and_comments
     @&& ifte right_bracket (fun _ ->
         fun ({ loc = endl; _ } as state) -> Ok (nil startl endl, state))
     @@ ifte (is expr_start)
          (fun _ ->
            (expr @&&& whitespace_and_comments) @>> fun expr ->
            list_tail (FT.singleton expr))
          (fun state ->
            Error
              {
                gotten =
                  Printf.sprintf "`%s`"
                    (BatSubstring.first state.remaining
                    |> Option.fold ~none:"EOF" ~some:(String.make 1));
                (* TODO: improve error message *)
                expected = "`]` or expression";
                region = Location.double state.loc.filename state.loc.coordinate;
              })

(** Grammar rules:

    func <- func_label (prolog_elements / karuta_elements)?

    prolog_elements <- left_paren whitespace_and_comments (right_paren /
    single_element prolog_elements_loop)

    prolog_elements_loop <- right_paren / comma whitespace_and_comments
    single_element prolog_elements_loop

    karuta_elements <- left_bracket whitespace_and_comments (right_bracket /
    single_element karuta_elements_loop)

    karuta_elements_loop <- right_bracket / comma whitespace_and_comments
    single_element karuta_elements_loop

    single_element <- expr whitespace_and_comments *)
and func : 'e. Ast.Expr.func Location.with_location parser =
 (* DONE *)
 (* TODO: create abstraction for elements *)
 fun ({ loc = startl; _ } as state) ->
  let single_element = expr @&&& whitespace_and_comments in
  let karuta_elements =
    let rec loop acc =
      ifte right_bracket (fun _ -> return acc)
      @@ ifte comma
           (fun _ ->
             (whitespace_and_comments @&& single_element) @>> fun element ->
             loop (FT.snoc acc element))
           (fun state ->
             Error
               {
                 gotten =
                   Printf.sprintf "`%s`"
                     (BatSubstring.first state.remaining
                     |> Option.fold ~none:"EOF" ~some:(String.make 1));
                 (* TODO: improve error message *)
                 expected = "`]` or `,`";
                 region =
                   Location.double state.loc.filename state.loc.coordinate;
               })
    in
    left_bracket @&& whitespace_and_comments
    @&& ifte right_bracket (fun _ -> return FT.empty)
    @@ ifte (is expr_start)
         (fun _ ->
           single_element @>> fun element -> loop (FT.singleton element))
         (fun state ->
           Error
             {
               gotten =
                 Printf.sprintf "`%s`"
                   (BatSubstring.first state.remaining
                   |> Option.fold ~none:"EOF" ~some:(String.make 1));
               (* TODO: improve error message *)
               expected = "`]` or expression";
               region = Location.double state.loc.filename state.loc.coordinate;
             })
  in
  let prolog_elements =
    let rec loop acc =
      ifte right_paren (fun _ -> return acc)
      @@ ifte comma
           (fun _ ->
             (whitespace_and_comments @&& single_element) @>> fun element ->
             loop (FT.snoc acc element))
           (fun state ->
             Error
               {
                 gotten =
                   Printf.sprintf "`%s`"
                     (BatSubstring.first state.remaining
                     |> Option.fold ~none:"EOF" ~some:(String.make 1));
                 (* TODO: improve error message *)
                 expected = "`)` or `,`";
                 region =
                   Location.double state.loc.filename state.loc.coordinate;
               })
    in
    left_paren @&& whitespace_and_comments
    @&& ifte right_paren (fun _ -> return FT.empty)
    @@ ifte (is expr_start)
         (fun _ ->
           single_element @>> fun element -> loop (FT.singleton element))
         (fun state ->
           Error
             {
               gotten =
                 Printf.sprintf "`%s`"
                   (BatSubstring.first state.remaining
                   |> Option.fold ~none:"EOF" ~some:(String.make 1));
               (* TODO: improve error message *)
               expected = "`)` or expression";
               region = Location.double state.loc.filename state.loc.coordinate;
             })
  in
  state
  |> func_label @>> fun label ->
     ifte (is left_paren) (fun _ ->
         prolog_elements @>> fun args ->
         fun ({ loc = endl; _ } as state) ->
          Ok
            ( Location.delimit startl endl (Ast.Expr.func label.content args),
              state ))
     @@ ifte (is left_bracket) (fun _ ->
         karuta_elements @>> fun args ->
         fun ({ loc = endl; _ } as state) ->
          Ok
            ( Location.delimit startl endl (Ast.Expr.func label.content args),
              state ))
     @@ return (Location.fmap Ast.Expr.atom label)

(* TODO: for the composites below, the location around the result should encompass
   the entire text, not just the first element *)

(** Grammar rule:

    query <- question_mark / comma whitespace_and_comments func
    whitespace_and_comments query *)
let query :
    'e.
    Ast.Expr.func Location.with_location ->
    Ast.Expr.func Location.with_location FT.t Location.with_location parser =
 (* DONE *)
 fun first_element ->
  let rec query_loop elements =
    ifte question_mark (fun _ -> return elements)
    @@ ifte comma
         (fun _ ->
           (whitespace_and_comments @&& func @&&& whitespace_and_comments)
           @>> fun func -> query_loop (FT.snoc elements func))
         (fun state ->
           Error
             {
               gotten =
                 Printf.sprintf "`%s`"
                   (BatSubstring.first state.remaining
                   |> Option.fold ~none:"EOF" ~some:(String.make 1));
               (* TODO: improve error message *)
               expected = "`?` or `,`";
               region = Location.double state.loc.filename state.loc.coordinate;
             })
  in
  query_loop (FT.singleton first_element)
  |> parser_map (fun l -> Location.add_loc l first_element.loc)

(** Grammar rules:

    declaration <- period / holds whitespace_and_comments func
    whitespace_and_comments declaration_body

    declaration_body <- period / comma whitespace_and_comments func
    whitespace_and_comments declaration_body *)
let declaration :
    'e.
    Ast.Expr.func Location.with_location ->
    Ast.ParserClause.decl Location.with_location parser =
 (* DONE *)
 fun { Location.content = head; loc } ->
  let rec body_loop elements =
    ifte period (fun _ -> return elements)
    @@ ifte comma
         (fun _ ->
           (whitespace_and_comments @&& func @&&& whitespace_and_comments)
           @>> fun func -> body_loop (FT.snoc elements func))
         (fun state ->
           Error
             {
               gotten =
                 Printf.sprintf "`%s`"
                   (BatSubstring.first state.remaining
                   |> Option.fold ~none:"EOF" ~some:(String.make 1));
               (* TODO: improve error message *)
               expected = "`.` or `,`";
               region = Location.double state.loc.filename state.loc.coordinate;
             })
  in
  ifte period (fun _ -> return FT.empty)
  @@ ifte holds
       (fun _ ->
         (whitespace_and_comments @&& func @&&& whitespace_and_comments)
         @>> fun func -> body_loop (FT.singleton func))
       (fun state ->
         (* TODO: this is actually unreachable, since in `clauses` we lookahead for first tokens of declaration and query,
         it's not the same case in `query` tho, since query is recursive  *)
         Error
           {
             gotten =
               Printf.sprintf "`%s`"
                 (BatSubstring.first state.remaining
                 |> Option.fold ~none:"EOF" ~some:(String.make 1));
             (* TODO: improve error message *)
             expected = "`.` or `:-`";
             region = Location.double state.loc.filename state.loc.coordinate;
           })
  |> parser_map (fun body ->
      Location.add_loc { Ast.ParserClause.head; body } loc)

(** Grammar rule:

    clauses <- !(holds / func) / holds whitespace_and_comments directive
    whitespace_and_comments clauses / func whitespace_and_comments (query /
    declaration) whitespace_and_comments clauses *)
let rec clauses : 'e. Ast.ParserClause.t FT.t -> Ast.ParserClause.t FT.t parser
    =
 (* DONE *)
 (* TODO: make it so that clauses doesn't need to take an ACC externally *)
 fun acc state ->
  state
  |> ifte holds (fun () ->
         (whitespace_and_comments @&& directive @&&& whitespace_and_comments)
         @>> fun directive ->
         let clause = Location.fmap Ast.ParserClause.directive directive in
         clauses (FT.snoc acc clause))
     @@ ifte
          (is @@ char_predicate is_func_start "unreachable")
          (fun _ ->
            func @>> fun func ->
            (whitespace_and_comments
            @&& ifte
                  (is @@ question_mark @|| comma)
                  (fun () ->
                    query func
                    |> parser_map @@ Location.fmap Ast.ParserClause.query)
                  (ifte
                     (is @@ holds @|| period)
                     (fun () ->
                       declaration func
                       |> parser_map
                          @@ Location.fmap Ast.ParserClause.declaration)
                     (fun state ->
                       Error
                         {
                           gotten =
                             Printf.sprintf "`%s`"
                               (BatSubstring.first state.remaining
                               |> Option.fold ~none:"EOF" ~some:(String.make 1)
                               );
                           (* TODO: improve error message, maybe describing the syntax *)
                           expected = "`?` or `,` or `:-` or `.`";
                           region =
                             Location.double state.loc.filename
                               state.loc.coordinate;
                         }))
            @&&& whitespace_and_comments)
            @>> fun clause -> clauses (FT.snoc acc clause))
     @@ return acc

(** Grammar rule:

    directive <- func whitespace_and_comments directive_bodies

    directive_bodies <- period / left_curly_brace top_level right_curly_brace
    whitespace_and_comments directive_bodies *)
and directive :
    'e.
    (Ast.Expr.func Location.with_location
    * Ast.ParserClause.t FT.t Location.with_location FT.t)
    Location.with_location
    parser =
  (* DONE *)
  let rec directive_bodies bodies_acc =
    ifte period (fun _ -> return bodies_acc)
    @@ ifte left_curly_brace
         (fun _ ->
           top_level @>> fun body ->
           fun state ->
            state
            |> (right_curly_brace
               |> parser_map_error (fun _ ->
                   {
                     gotten =
                       Printf.sprintf "`%s`"
                         (BatSubstring.first state.remaining
                         |> Option.fold ~none:"EOF" ~some:(String.make 1));
                     (* TODO: improve error message *)
                     expected = "`}` or `:-` or FUNC";
                     region =
                       Location.double state.loc.filename state.loc.coordinate;
                   }))
               @&& whitespace_and_comments
               @&& directive_bodies (FT.snoc bodies_acc body))
         (fun state ->
           Error
             {
               gotten =
                 Printf.sprintf "`%s`"
                   (BatSubstring.first state.remaining
                   |> Option.fold ~none:"EOF" ~some:(String.make 1));
               (* TODO: improve error message *)
               expected = "`.` or `{`";
               region = Location.double state.loc.filename state.loc.coordinate;
             })
  in
  fun state ->
    state
    |> func @>> fun header ->
       whitespace_and_comments @&& directive_bodies FT.empty
       @>> fun bodies ->
       return (Location.add_loc (header, bodies) Location.(header.loc))

(** Grammar rule:

    top_level <- whitespace_and_comments clauses *)
(* NOTE: since clauses doesn't have an explicit terminator, when the terminator after top_level/clauses fail, the error should mention that besides the terminator being expected, the start of clauses/top_level also was expected *)
and top_level : 'e. Ast.ParserClause.t FT.t Location.with_location parser =
 (* DONE *)
 fun ({ loc = startl; _ } as state) ->
  state
  |> (whitespace_and_comments @&& clauses FT.empty) @>> fun clauses ->
     fun ({ loc = endl; _ } as state) ->
      Ok (Location.delimit startl endl clauses, state)

(** Grammar rule:

    start <- top_level !. *)
let parse (filename : string) (source : string) =
  (* DONE *)
  match
    {
      remaining = BatSubstring.all source;
      loc = { filename; coordinate = Location.origin };
    }
    |> top_level @&&& fun state ->
       state
       |> (is_not some
          |> parser_map_error (fun _ ->
              {
                gotten =
                  Printf.sprintf "`%s`"
                    (BatSubstring.first state.remaining
                    |> Option.get |> String.make 1);
                (* TODO: improve error message *)
                expected = "EOF or `:-` or FUNC";
                region = Location.double state.loc.filename state.loc.coordinate;
              }))
  with
  | Ok (parsed, _) -> parsed.content
  | Error { region; gotten; expected } ->
      Logger.error region
        (Printf.sprintf "Expected %s, but got %s" expected gotten);
      raise Exit

(* TODO: bind needs lower precedence than sequencing *)
(* TODO: logger might have a off by one error when printing the line and characters highlighted *)
(* TODO: logger needs to print what kind of error it is *)
(* TODO: abstract `ParserError creation *)
(* TODO: remember to treat func as non-terminal, that is, with the
same pattern of checking the first char or literal and commiting or not *)
(* TODO: variables right now can have a lowercase right after underscore
make it so it needs to be uppercase *)
(* TODO: parse a more broad syntax to give better error, for example 'foo':bar, that is error that people are likely to make and specially ones that seem it could be valid, specially when no space are present, which would cause something with no space to be parsed in a non obvious way as two things, it's the same kind of thing as parsing the longest match  *)
