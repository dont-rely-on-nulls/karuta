let from_declaration (clause : Ast.parser_clause) :
    Ast.decl Location.with_location =
  match clause with
  | { content = Declaration decl; loc } -> { content = decl; loc }
  | _ ->
      Logger.simply_unreachable "unreachable from_declaration";
      exit 1

let rec remove_comments (clause : Ast.parser_clause) : Ast.parser_clause option
    =
  let open Ast in
  let open Location in
  let non_comment { content = func; _ } =
    match func with { namef = "comment"; _ } -> false | _ -> true
  in
  match clause with
  | { content = Declaration { head = { namef = "comment"; _ }; _ }; _ } -> None
  | { content = Declaration { head; body }; _ } as decl ->
      Some
        {
          decl with
          content = Declaration { head; body = body |> List.filter non_comment };
        }
  | {
   content = QueryConjunction [ { content = { namef = "comment"; _ }; _ } ];
   _;
  } ->
      None
  | { content = QueryConjunction [ _ ]; _ } as query -> Some query
  | { content = QueryConjunction []; _ } -> None
  | { content = QueryConjunction queries; loc } ->
      let filtered_queries =
        List.map
          (fun query ->
            remove_comments
            @@ { content = Ast.QueryConjunction [ query ]; loc = query.loc })
          queries
      in
      filtered_queries
      |> List.concat_map Option.to_list
      |> List.map (function
           | { content = Ast.QueryConjunction [ func ]; _ } -> func
           | _ ->
               Logger.simply_unreachable
                 "The conjunctions we constructed are guaranteed not to have \
                  this form.";
               exit 1)
      |> fun funcs ->
      if List.is_empty funcs then None
      else Some { content = Ast.QueryConjunction funcs; loc }

let show_clauses (clauses : Ast.clause list) : string =
  List.fold_left (fun acc term -> acc ^ "\n" ^ Ast.show_clause term) "" clauses
[@@warning "-32"]

let check_empty_heads (clause : Ast.parser_clause) : Ast.parser_clause =
  match clause with
  | { content = Declaration { head = { namef = ""; _ }; _ }; loc } ->
      Logger.error loc
        "You cannot have a query or declaration with an empty name";
      exit 1
  | other -> other

module S = BatSet

type variable_set = Ast.var BatSet.t

let rec find_variables (element : Ast.expr') : variable_set =
  let open Location in
  match element with
  | Ast.Variable var -> S.add var S.empty
  | Ast.Functor { elements = more_elements; _ } ->
      List.fold_left
        (fun acc element -> S.union acc (find_variables @@ strip_loc element))
        S.empty more_elements
  | _ -> S.empty

let parser_to_compiler (clause : Ast.parser_clause) : Ast.clause list =
  let open Location in
  match clause with
  | { content = Declaration decl; loc } ->
      [ { content = MultiDeclaration (decl, []); loc } ]
  | { content = QueryConjunction funcs; loc } ->
      let folder set func =
        S.union set (find_variables @@ Ast.Functor (strip_loc func))
      in
      let variables = List.fold_left folder S.empty funcs in
      let list_variables = S.to_list variables in
      let head : Ast.func =
        {
          namef = "";
          elements =
            List.map
              (fun var -> { content = Ast.Variable var; loc })
              list_variables;
          arity = S.cardinal variables;
        }
      in
      let declaration =
        { content = Ast.MultiDeclaration ({ head; body = funcs }, []); loc }
      in
      let query = { content = Ast.Query head; loc } in
      [ declaration; query ]

let group_clauses (clauses : Ast.parser_clause list) :
    Ast.clause list * (Ast.tag * int) BatSet.t =
  let compare_func (f1 : Ast.func) (f2 : Ast.func) : int =
    if f1.namef = f2.namef && f1.arity = f2.arity then 0 else 1
  in
  let compare_clauses (c1 : Ast.parser_clause) (c2 : Ast.parser_clause) : int =
    match (c1, c2) with
    | ( { content = Declaration { head = h1; _ }; _ },
        { content = Declaration { head = h2; _ }; _ } ) ->
        compare_func h1 h2
    | _, _ -> 1
  in
  let multi_mapper (group : Ast.parser_clause list) : Ast.clause list =
    match group with
    | [ x ] -> parser_to_compiler x
    | { content = Declaration first; loc } :: many ->
        [
          {
            content =
              Ast.MultiDeclaration (first, List.map from_declaration many);
            loc;
          };
        ]
    | _ ->
        Logger.simply_unreachable "unreachable group";
        exit 1
  in
  let collect_definitions (clauses : Ast.clause list) : (Ast.tag * int) BatSet.t
      =
    List.fold_left
      (fun set clause ->
        match Location.strip_loc clause with
        | Ast.MultiDeclaration ({ head = { namef; arity; _ }; _ }, _) ->
            BatSet.add (namef, arity) set
        | _ -> set)
      BatSet.empty clauses
  in
  let open Batteries in
  let grouped_clauses =
    clauses
    |> List.filter_map remove_comments
    |> List.map check_empty_heads |> List.group compare_clauses
    |> List.concat_map multi_mapper
  in
  (grouped_clauses, collect_definitions grouped_clauses)
