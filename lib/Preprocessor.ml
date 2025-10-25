let from_declaration (clause : Ast.parser_clause) : Ast.decl =
  match clause with
  | Declaration decl -> decl
  | _ -> failwith "unreachable from_declaration"

let rec remove_comments (clause : Ast.parser_clause) : Ast.parser_clause option
    =
  let open Ast in
  let non_comment func =
    match func with { namef = "comment"; _ } -> false | _ -> true
  in
  match clause with
  | Declaration { head = { namef = "comment"; _ }; _ } -> None
  | Declaration { head; body } ->
      Some (Declaration { head; body = body |> List.filter non_comment })
  | QueryConjunction [ { namef = "comment"; _ } ] -> None
  | QueryConjunction [ _ ] as query -> Some query
  | QueryConjunction [] -> None
  | QueryConjunction queries ->
      let filtered_queries =
        List.map
          (fun query -> remove_comments @@ Ast.QueryConjunction [ query ])
          queries
      in
      filtered_queries
      |> List.concat_map Option.to_list
      |> List.map (fun (Ast.QueryConjunction [ func ]) -> func)
      |> fun funcs ->
      if List.is_empty funcs then None else Some (Ast.QueryConjunction funcs)

let show_clauses (clauses : Ast.clause list) : string =
  List.fold_left (fun acc term -> acc ^ "\n" ^ Ast.show_clause term) "" clauses
[@@warning "-32"]

let check_empty_heads (clause : Ast.parser_clause) : Ast.parser_clause =
  match clause with
  | Declaration { head = { namef = ""; _ }; _ } ->
      failwith "You cannot have a query or declaration with an empty name"
  | other -> other

module S = BatSet

type variable_set = Ast.var BatSet.t

let rec find_variables (element : Ast.expr) : variable_set =
  match element with
  | Ast.Variable var -> S.add var S.empty
  | Ast.Functor { elements = more_elements; _ } ->
      List.fold_left
        (fun acc element -> S.union acc (find_variables element))
        S.empty more_elements
  | _ -> S.empty

let parser_to_compiler (clause : Ast.parser_clause) : Ast.clause list =
  match clause with
  | Declaration decl -> [ MultiDeclaration (decl, []) ]
  | QueryConjunction funcs ->
      let folder set func = S.union set (find_variables @@ Ast.Functor func) in
      let variables = List.fold_left folder S.empty funcs in
      let list_variables = S.to_list variables in
      let head : Ast.func =
        {
          namef = "";
          elements = List.map (fun var -> Ast.Variable var) list_variables;
          arity = S.cardinal variables;
        }
      in
      let declaration = Ast.MultiDeclaration ({ head; body = funcs }, []) in
      let query = Ast.Query head in
      [ declaration; query ]

let group_clauses (clauses : Ast.parser_clause list) : Ast.clause list =
  let compare_func (f1 : Ast.func) (f2 : Ast.func) : int =
    if f1.namef = f2.namef && f1.arity = f2.arity then 0 else 1
  in
  let compare_clauses (c1 : Ast.parser_clause) (c2 : Ast.parser_clause) : int =
    match (c1, c2) with
    | Declaration { head = h1; _ }, Declaration { head = h2; _ } ->
        compare_func h1 h2
    | _, _ -> 1
  in
  let multi_mapper (group : Ast.parser_clause list) : Ast.clause list =
    match group with
    | [ x ] -> parser_to_compiler x
    | Declaration first :: many ->
        [ Ast.MultiDeclaration (first, List.map from_declaration many) ]
    | _ -> failwith "unreachable group"
  in
  let open Batteries in
  clauses
  |> List.filter_map remove_comments
  |> List.map check_empty_heads |> List.group compare_clauses
  |> List.concat_map multi_mapper
