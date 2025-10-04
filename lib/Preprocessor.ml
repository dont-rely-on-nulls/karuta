let from_declaration (clause : Ast.parser_clause) : Ast.decl =
  match clause with
  | Declaration decl -> decl
  | _ -> failwith "unreachable from_declaration"

let remove_comments (clause : Ast.parser_clause) : Ast.parser_clause option =
  let open Ast in
  let non_comment func =
    match func with { namef = "comment"; _ } -> false | _ -> true
  in
  match clause with
  | Declaration { head = { namef = "comment"; _ }; _ } -> None
  | Declaration { head; body } ->
      Some (Declaration { head; body = body |> List.filter non_comment })
  | Query { namef = "comment"; _ } -> None
  | Query _ as query -> Some query

let show_clauses (clauses : Ast.clause list) : string =
  List.fold_left (fun acc term -> acc ^ "\n" ^ Ast.show_clause term) "" clauses
[@@warning "-32"]

let parser_to_compiler (clause : Ast.parser_clause) : Ast.clause =
  match clause with
  | Declaration decl -> MultiDeclaration (decl, [])
  | Query func -> QueryConjunction func

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
  let multi_mapper (group : Ast.parser_clause list) : Ast.clause =
    match group with
    | [ x ] -> parser_to_compiler x
    | Declaration first :: many ->
        Ast.MultiDeclaration (first, List.map from_declaration many)
    | _ -> failwith "unreachable group"
  in
  let open Batteries in
  clauses
  |> List.filter_map remove_comments
  |> List.group compare_clauses |> List.map multi_mapper
