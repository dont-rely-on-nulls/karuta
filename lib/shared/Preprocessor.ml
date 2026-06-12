let all_atoms (args : Ast.Expr.t FT.t) : unit =
  match
    args
    |> FT.find_opt
       @@ Fun.compose
            (function
              | Ast.Expr.Functor { elements; _ } when FT.is_empty elements ->
                  false
              | _ -> true)
            Location.strip_loc
  with
  | None -> ()
  | Some { loc; _ } ->
      Logger.error loc "Expected atom.";
      exit 1

let compare_clauses (c1 : Ast.ParserClause.t) (c2 : Ast.ParserClause.t) : int =
  match (c1, c2) with
  | ( { content = Declaration { head = h1; _ }; _ },
      { content = Declaration { head = h2; _ }; _ } ) ->
      Ast.Expr.compare_func h1 h2
  | _, _ -> -1

let decl_header ({ head; _ } : Ast.ParserClause.decl) : Ast.head =
  { name = Ast.Expr.extract_func_label head; arity = FT.size head.elements }

let rec remove_comments (clause : Ast.ParserClause.t) :
    Ast.ParserClause.t option =
  let open Ast in
  let open Location in
  let non_comment call =
    match call.content with
    | Expr.Functor func when Ast.Expr.match_func func [ "karuta"; "comment" ] ->
        false
    | _ -> true
  in
  match clause with
  | { content = ParserClause.Directive ({ content = header; _ }, _); _ }
    when Ast.Expr.match_func header [ "comment" ] ->
      None
  | { content = ParserClause.Directive (head, body); loc } ->
      Some
        {
          content =
            ParserClause.Directive
              (head, FT.map (FT.filter_map remove_comments) body);
          loc;
        }
  | { content = Declaration { head; _ }; _ }
    when Ast.Expr.match_func head [ "comment" ] ->
      None
  | { content = Declaration { head; body }; _ } as decl ->
      Some
        {
          decl with
          content =
            Declaration
              {
                head;
                body =
                  body
                  |> FT.filter
                     @@ Fun.compose non_comment
                          (Location.fmap (fun v -> Ast.Expr.Functor v));
              };
        }
  | { content = QueryConjunction q; _ } as query when FT.size q = 1 ->
      Some query
  | { content = QueryConjunction q; _ } when FT.is_empty q -> None
  | { content = QueryConjunction queries; loc } ->
      let filtered_queries =
        FT.map
          (fun query ->
            remove_comments
            @@ {
                 content =
                   Ast.ParserClause.QueryConjunction (FT.singleton query);
                 loc = query.loc;
               })
          queries
      in
      filtered_queries |> FT.concat_map FT.of_option
      |> FT.map (function
        | { content = Ast.ParserClause.QueryConjunction q; _ }
          when FT.size q = 1 ->
            FT.head_exn q
        | _ ->
            Logger.simply_unreachable
              "The conjunctions we constructed are guaranteed not to have this \
               form.";
            exit 1)
      |> fun funcs ->
      if FT.is_empty funcs then None
      else Some { content = Ast.ParserClause.QueryConjunction funcs; loc }

let check_empty_heads (clause : Ast.ParserClause.t) : Ast.ParserClause.t =
  match clause with
  | {
   content = Declaration { head = { name = _, { content = ""; _ }; _ }; _ };
   loc;
  } ->
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
      FT.fold_left
        (fun acc element -> S.union acc (find_variables @@ strip_loc element))
        S.empty more_elements
  | _ -> S.empty

type t = { filename : string; dependencies : DependencyGraph.t }

let initialize filename : t = { filename; dependencies = BatMap.String.empty }

type ('directives, 'mods) output = {
  dependencies : DependencyGraph.t;
  clauses : ('directives, 'mods) Ast.Clause.t BatFingerTree.t;
}

type ('directives, 'mods) group =
  t -> Ast.ParserClause.t FT.t -> ('directives, 'mods) output

let map_clauses f o = { o with clauses = f o.clauses }

let merge :
    'directives 'mods.
    ('directives, 'mods) output ->
    ('directives, 'mods) output ->
    ('directives, 'mods) output =
 fun l r ->
  {
    dependencies = DependencyGraph.merge l.dependencies r.dependencies;
    clauses = BatFingerTree.append l.clauses r.clauses;
  }

let fold_map :
    'directives 'mods.
    DependencyGraph.t ->
    (Ast.ParserClause.t FT.t -> ('directives, 'mods) output) ->
    Ast.ParserClause.t FT.t FT.t ->
    ('directives, 'mods) output =
 fun dependencies f grouped_clauses ->
  FT.fold_left
    (fun acc elem -> merge acc @@ f elem)
    { dependencies; clauses = BatFingerTree.empty }
    grouped_clauses

module type PREPROCESSOR_CONFIG = sig
  type directives
  type mods

  val preprocess_clause :
    (directives, mods) group ->
    t ->
    Ast.ParserClause.t ->
    (directives, mods) output

  val renamer : Ast.ParserClause.decl -> Ast.Clause.decl
end

module type PREPROCESSOR = sig
  type directives
  type mods

  val preprocess_clauses :
    t -> Ast.ParserClause.t FT.t -> (directives, mods) output
end

module Make (Config : PREPROCESSOR_CONFIG) :
  PREPROCESSOR
    with type directives = Config.directives
    with type mods = Config.mods = struct
  include Config

  let from_declaration (clause : Ast.ParserClause.t) :
      Ast.Clause.decl Location.with_location =
    match clause with
    | { content = Declaration decl; loc } ->
        { content = Config.renamer decl; loc }
    | _ ->
        Logger.simply_unreachable "unreachable from_declaration";
        exit 1

  let rec preprocess_clauses ({ dependencies; _ } as preprocessor : t)
      (clauses : Ast.ParserClause.t FT.t) =
    let multi_mapper (group : Ast.ParserClause.t FT.t) =
      match FT.front group with
      | Some (remaining, single) when remaining = FT.empty ->
          Config.preprocess_clause preprocess_clauses preprocessor single
      | Some (many, { content = Declaration first; loc }) ->
          {
            dependencies;
            clauses =
              FT.singleton
                {
                  Location.content =
                    Ast.Clause.MultiDeclaration
                      ( decl_header first,
                        Config.renamer first,
                        FT.map from_declaration many );
                  loc;
                };
          }
      | _ ->
          Logger.simply_unreachable "unreachable group";
          exit 1
    in
    clauses
    |> FT.filter_map remove_comments
    |> FT.map check_empty_heads |> FT.group compare_clauses
    |> fold_map preprocessor.dependencies multi_mapper
end
