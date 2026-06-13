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

type ('directives, 'mods) one_output =
  | Directive of ('directives, 'mods) Ast.Module.directive
  | TargetSpecific of {
      update : 'mods -> 'mods;
      dependencies : BatSet.String.t;
    }

type ('directives, 'mods) output = {
  dependencies : DependencyGraph.t;
  module_ : ('directives, 'mods) Ast.Module.module_body;
}

type ('directives, 'mods) group =
  t -> Ast.ParserClause.t FT.t -> ('directives, 'mods) output

let map_module f o = { o with module_ = f o.module_ }

module type PREPROCESSOR_CONFIG = sig
  type directives
  type mods

  val initial_mods : unit -> mods

  val preprocess_directive :
    (directives, mods) group ->
    t ->
    Ast.ParserClause.directive ->
    (directives, mods) one_output

  val renamer : Ast.ParserClause.decl -> Ast.Module.decl
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

  let rec preprocess_clauses ({ dependencies; filename } : t)
      (clauses : Ast.ParserClause.t FT.t) =
    let split ({ dependencies; module_ } : (directives, mods) output)
        (parser_clause : Ast.ParserClause.t) : (directives, mods) output =
      let open Location in
      let { loc; content } = parser_clause in
      match content with
      | QueryConjunction calls when module_.query = None ->
          let open Ast in
          let folder set call =
            S.union set (find_variables @@ Expr.Functor (strip_loc call))
          in
          let variables = FT.fold_left folder S.empty calls in
          let list_variables = FT.of_list @@ S.to_list variables in
          let query_name = "" in
          let head : Expr.func =
            {
              name = (FT.empty, { content = query_name; loc });
              elements =
                FT.map
                  (fun var -> { content = Expr.Variable var; loc })
                  list_variables;
            }
          in
          let fake_decl : ParserClause.decl = { head; body = calls } in
          let declaration_body =
            ({ content = renamer fake_decl; loc }, FT.empty)
          in
          let query : Module.query_ref with_location =
            { content = { name = query_name; args = list_variables }; loc }
          in
          {
            dependencies;
            module_ =
              {
                module_ with
                query = Some query;
                declarations =
                  BatMap.add (decl_header fake_decl) declaration_body
                    module_.declarations;
              };
          }
      | QueryConjunction _ ->
          Logger.error loc "Modules can have at most one query";
          Logger.error (Option.get module_.query).loc "First query here";
          exit 1
      | Declaration decl ->
          let renamed = { loc; content = renamer decl } in
          {
            dependencies;
            module_ =
              {
                module_ with
                declarations =
                  BatMap.modify_opt (decl_header decl)
                    (function
                      | None -> Some (renamed, FT.empty)
                      | Some (first, others) ->
                          Some (first, FT.snoc others renamed))
                    module_.declarations;
              };
          }
      | Directive (({ loc; content = header }, _bodies) as directive) -> (
          if Ast.Expr.match_func header [ "module" ] then failwith "TODO"
          else if Ast.Expr.match_func header [ "signature" ] then
            failwith "TODO"
          else
            match
              Config.preprocess_directive preprocess_clauses
                { dependencies; filename } directive
            with
            | Directive directive ->
                {
                  dependencies;
                  module_ =
                    {
                      module_ with
                      directives =
                        FT.snoc module_.directives
                        @@ Location.add_loc directive loc;
                    };
                }
            | TargetSpecific { dependencies = dependency_set; update } ->
                {
                  dependencies =
                    BatMap.String.add module_.name.content dependency_set
                      dependencies;
                  module_ =
                    {
                      module_ with
                      target_specific = update module_.target_specific;
                    };
                })
    in
    clauses
    |> FT.filter_map remove_comments
    |> FT.map check_empty_heads
    |> FT.fold_left split
         {
           dependencies;
           module_ =
             {
               name =
                 Location.add_loc
                   (ModuleName.of_filepath filename)
                   Location.dummy;
               signature = None;
               declarations = BatMap.empty;
               directives = FT.empty;
               target_specific = Config.initial_mods ();
               query = None;
             };
         }
end
