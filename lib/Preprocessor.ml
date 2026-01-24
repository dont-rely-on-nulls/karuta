let rename_arg ({ loc; _ } as expr : Ast.Expr.t) (counter : int) :
    Ast.Expr.call Location.with_location =
  let open Ast in
  let new_var = Location.add_loc (Expr.Variable (string_of_int counter)) loc in
  {
    content =
      Expr.Qualified
        ( { content = "karuta"; loc },
          Expr.Unqualified
            {
              content = { name = "eq"; elements = [ new_var; expr ]; arity = 2 };
              loc;
            } );
    loc;
  }

let compare_func (f1 : Ast.Expr.func) (f2 : Ast.Expr.func) : int =
  Ast.Clause.compare_head
    { name = f1.name; arity = f1.arity }
    { name = f2.name; arity = f2.arity }

let compare_clauses (c1 : Ast.ParserClause.t) (c2 : Ast.ParserClause.t) : int =
  match (c1, c2) with
  | ( { content = Declaration { head = h1; _ }; _ },
      { content = Declaration { head = h2; _ }; _ } ) ->
      compare_func h1 h2
  | _, _ -> -1

let canonical_order (l : Ast.Clause.t) (r : Ast.Clause.t) : int =
  let open Ast.Clause in
  match (Location.strip_loc l, Location.strip_loc r) with
  | Directive _, Directive _ -> 0
  | Directive _, _ -> -1
  | _, Directive _ -> 1
  | _ -> 0

let decl_header ({ head = { name; arity; _ }; _ } : Ast.ParserClause.decl) :
    Ast.Clause.head =
  { name; arity }

let rename_declaration
    ({ head = { elements; arity; _ }; body } : Ast.ParserClause.decl) :
    Ast.Clause.decl =
  let _, new_body =
    List.fold_right
      (fun arg (counter, body') ->
        let to_append = rename_arg arg counter in
        (counter - 1, to_append :: body'))
      elements
      (arity - 1, body)
  in
  { body = new_body; original_arg_list = elements }

let from_declaration (clause : Ast.ParserClause.t) :
    Ast.Clause.decl Location.with_location =
  match clause with
  | { content = Declaration decl; loc } ->
      { content = rename_declaration decl; loc }
  | _ ->
      Logger.simply_unreachable "unreachable from_declaration";
      exit 1

let rec remove_comments (clause : Ast.ParserClause.t) :
    Ast.ParserClause.t option =
  let open Ast in
  let open Location in
  let non_comment call =
    match call with
    | {
     content =
       Expr.Qualified
         ( { content = "karuta"; _ },
           Expr.Unqualified { content = { Expr.name = "comment"; _ }; _ } );
     _;
    } ->
        false
    | _ -> true
  in
  match clause with
  | { content = ParserClause.Directive ({ name = "comment"; _ }, _); _ } -> None
  | { content = ParserClause.Directive (head, body); loc } ->
      Some
        {
          content =
            ParserClause.Directive (head, List.filter_map remove_comments body);
          loc;
        }
  | { content = Declaration { head = { name = "comment"; _ }; _ }; _ } -> None
  | { content = Declaration { head; body }; _ } as decl ->
      Some
        {
          decl with
          content = Declaration { head; body = body |> List.filter non_comment };
        }
  | { content = QueryConjunction queries; loc } ->
      let filtered_queries =
        List.map
          (fun query ->
            remove_comments
            @@ {
                 content = Ast.ParserClause.QueryConjunction [ query ];
                 loc = query.loc;
               })
          queries
      in
      filtered_queries
      |> List.concat_map Option.to_list
      |> List.map (function
           | { content = Ast.ParserClause.QueryConjunction [ func ]; _ } -> func
           | _ ->
               Logger.simply_unreachable
                 "The conjunctions we constructed are guaranteed not to have \
                  this form.";
               exit 1)
      |> fun funcs ->
      if List.is_empty funcs then None
      else Some { content = Ast.ParserClause.QueryConjunction funcs; loc }

let show_clauses (clauses : Ast.Clause.t list) : string =
  List.fold_left (fun acc term -> acc ^ "\n" ^ Ast.Clause.show term) "" clauses
[@@warning "-32"]

let check_empty_heads (clause : Ast.ParserClause.t) : Ast.ParserClause.t =
  match clause with
  | { content = Declaration { head = { name = ""; _ }; _ }; loc } ->
      Logger.error loc
        "You cannot have a query or declaration with an empty name";
      exit 1
  | other -> other

module S = BatSet

type variable_set = string BatSet.t

let rec find_variables (element : Ast.Expr.base) : variable_set =
  let open Location in
  let open Ast.Expr in
  match element with
  | Variable var -> S.add var S.empty
  | Functor { elements = more_elements; _ } ->
      List.fold_left
        (fun acc element -> S.union acc (find_variables @@ strip_loc element))
        S.empty more_elements
  | _ -> S.empty

let rec func_of_call : Ast.Expr.call -> Ast.Expr.func = function
  | Ast.Expr.Qualified (_, more) -> func_of_call more
  | Ast.Expr.Unqualified func -> func.content

let rec parser_to_compiler (clause : Ast.ParserClause.t) : Ast.Clause.t list =
  let open Location in
  let open Ast in
  match clause with
  | { content = Directive (head, body); loc } ->
      [ { content = Directive (head, group_clauses body); loc } ]
  | { content = Declaration decl; loc } ->
      [
        {
          content =
            MultiDeclaration (decl_header decl, rename_declaration decl, []);
          loc;
        };
      ]
  | { content = QueryConjunction calls; loc } ->
      let folder set call =
        S.union set
          (find_variables
          @@ Expr.Functor ((Fun.compose func_of_call strip_loc) call))
      in
      let variables = List.fold_left folder S.empty calls in
      let list_variables = S.to_list variables in
      let query_name = "" in
      let head : Expr.func =
        {
          name = query_name;
          elements =
            List.map
              (fun var -> { content = Expr.Variable var; loc })
              list_variables;
          arity = S.cardinal variables;
        }
      in
      let fake_decl = { ParserClause.head; body = calls } in
      let declaration =
        {
          content =
            Clause.MultiDeclaration
              (decl_header fake_decl, rename_declaration fake_decl, []);
          loc;
        }
      in
      let query =
        {
          content =
            Clause.Query
              { name = query_name; arity = head.arity; args = list_variables };
          loc;
        }
      in
      [ declaration; query ]

and group_clauses (clauses : Ast.ParserClause.t list) : Ast.Clause.t list =
  let multi_mapper (group : Ast.ParserClause.t list) : Ast.Clause.t list =
    match group with
    | [ x ] -> parser_to_compiler x
    | { content = Declaration first; loc } :: many ->
        [
          {
            content =
              Ast.Clause.MultiDeclaration
                ( decl_header first,
                  rename_declaration first,
                  List.map from_declaration many );
            loc;
          };
        ]
    | _ ->
        Logger.simply_unreachable "unreachable group";
        exit 1
  in
  let open Batteries in
  clauses
  |> List.filter_map remove_comments
  |> List.map check_empty_heads |> List.group compare_clauses
  |> List.concat_map multi_mapper
  |> List.sort canonical_order
