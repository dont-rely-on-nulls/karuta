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
              (head, FT.map (Location.fmap (FT.filter_map remove_comments)) body);
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
  | Cons (lhs, rhs) ->
      S.union (find_variables lhs.content) (find_variables rhs.content)
  | Nil | Integer _ -> S.empty

type t = { filename : string; dependencies : DependencyGraph.t }

let initialize filename : t = { filename; dependencies = BatMap.String.empty }

type ('directives, 'mods) one_output =
  | TargetSpecificDirective of 'directives
  | Update of { action : 'mods -> 'mods; dependencies : BatSet.String.t }

type ('directives, 'mods) output = {
  dependencies : DependencyGraph.t;
  module_ : ('directives, 'mods) Ast.Module.module_body;
}

type ('directives, 'mods) recur =
  t -> Ast.ParserClause.t FT.t -> ('directives, 'mods) output

let map_module f o = { o with module_ = f o.module_ }

module type PREPROCESSOR_CONFIG = sig
  type directives
  type mods

  val init_mods : unit -> mods

  val preprocess_declaration :
    Ast.ParserClause.decl Location.with_location ->
    Ast.Module.multi_declaration_env ->
    Ast.Module.multi_declaration_env

  val preprocess_directive :
    (directives, mods) recur ->
    Ast.ParserClause.directive ->
    (directives, mods) one_output

  val preprocess_query :
    Location.location ->
    Ast.Expr.func Location.with_location FT.t ->
    (directives, mods) Ast.Module.module_body ->
    (directives, mods) Ast.Module.module_body
end

let group_declaration (name : string)
    (decl : Ast.Module.decl Location.with_location)
    (declarations : Ast.Module.multi_declaration_env) :
    Ast.Module.multi_declaration_env =
  BatMap.modify_opt
    { Ast.name; arity = FT.size decl.content.original_arg_list }
    (function
      | None -> Some (decl, FT.empty)
      | Some (first, others) -> Some (first, FT.snoc others decl))
    declarations

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
          { dependencies; module_ = Config.preprocess_query loc calls module_ }
      | QueryConjunction _ ->
          Logger.error loc "Modules can have at most one query";
          Logger.error (Option.get module_.query).loc "First query here";
          exit 1
      | Declaration decl ->
          {
            dependencies;
            module_ =
              {
                module_ with
                declarations =
                  Config.preprocess_declaration { content = decl; loc }
                    module_.declarations;
              };
          }
      | Directive (({ loc; content = header }, bodies) as directive) -> (
          all_atoms header.elements;
          let arity = FT.size header.elements in
          let check_for_type_annotations : Ast.ParserClause.t FT.t -> unit =
           fun clauses ->
            let all_underscores : Ast.Expr.t FT.t -> bool =
              FT.for_all Ast.Expr.is_underscore
            in
            let open Ast.ParserClause in
            FT.iter
              (function
                | {
                    content =
                      Ast.ParserClause.Declaration { head = { elements; _ }; _ };
                    loc;
                  } ->
                    if not @@ all_underscores elements then
                      Logger.warning loc
                        "Types are not supported yet. Ignoring argument types."
                | _ -> ())
              clauses
          in
          if Ast.Expr.match_func header [ "module" ] then (
            let module_name =
              match FT.front header.elements with
              | None ->
                  Logger.error loc "All modules must be named";
                  exit 1
              | Some (_, module_name) ->
                  Location.add_loc
                    (Ast.Expr.extract_functor_label module_name)
                    module_name.loc
            in
            match (arity, FT.front bodies) with
            | _, None ->
                Logger.error loc
                  "Expected a module to have at least one body for its \
                   implementation";
                exit 1
            | 1, Some (remaining, body) when FT.size remaining = 0 ->
                let { dependencies = body_dependencies; module_ = body_module }
                    =
                  preprocess_clauses { dependencies; filename } body.content
                in
                {
                  dependencies =
                    DependencyGraph.merge dependencies body_dependencies;
                  module_ =
                    {
                      module_ with
                      directives =
                        FT.snoc module_.directives
                        @@ Location.add_loc
                             (Ast.Module.Module
                                { body_module with name = module_name })
                             loc;
                    };
                }
            | 1, Some (remaining, signature_body) when FT.size remaining = 1 ->
                let body = FT.head_exn remaining in
                check_for_type_annotations signature_body.content;
                let { module_ = signature_module; _ } =
                  preprocess_clauses { dependencies; filename }
                    signature_body.content
                in
                let { dependencies = body_dependencies; module_ = body_module }
                    =
                  preprocess_clauses { dependencies; filename } body.content
                in
                {
                  dependencies =
                    DependencyGraph.merge dependencies body_dependencies;
                  module_ =
                    {
                      module_ with
                      directives =
                        FT.snoc module_.directives
                        @@ Location.add_loc
                             (Ast.Module.Module
                                {
                                  body_module with
                                  name = module_name;
                                  signature =
                                    Some
                                      (Location.add_loc
                                         (Ast.Module.Inlined
                                            {
                                              declarations =
                                                Ast.Module
                                                .multi_declaration_env_to_declaration_env
                                                  signature_module.declarations;
                                              directives =
                                                signature_module.directives;
                                            })
                                         signature_body.loc);
                                })
                             loc;
                    };
                }
            | 2, Some (remaining, body) when FT.size remaining = 0 ->
                let ({ loc = signature_name_loc; _ } as signature_name) =
                  FT.get header.elements 1
                in
                let signature_name =
                  Ast.Expr.get_functor_label signature_name
                in
                let { dependencies = body_dependencies; module_ = body_module }
                    =
                  preprocess_clauses { dependencies; filename } body.content
                in
                {
                  dependencies =
                    DependencyGraph.merge dependencies body_dependencies;
                  module_ =
                    {
                      module_ with
                      directives =
                        FT.snoc module_.directives
                        @@ Location.add_loc
                             (Ast.Module.Module
                                {
                                  body_module with
                                  name = module_name;
                                  signature =
                                    Some
                                      (Location.add_loc
                                         (Ast.Module.Named signature_name)
                                         signature_name_loc);
                                })
                             loc;
                    };
                }
            | 2, Some (remaining, _) when FT.size remaining = 1 ->
                Logger.error loc
                  "There is a conflict between the provided named signature \
                   and a provided inline signature. There can be only one!";
                exit 1
            | arity, Some _ when arity = 1 || arity = 2 ->
                Logger.error loc
                @@ "Modules can have at most only two bodies and you have "
                ^ Int.to_string @@ FT.size bodies;
                FT.error_all_after 2 bodies "Extra body for module here";
                exit 1
            | arity, _ ->
                Logger.error loc
                @@ "Module directives can handle at most 2 atoms as arguments \
                    and you have " ^ Int.to_string arity;
                FT.error_all_after 2 header.elements
                  "Extra module argument here";
                exit 1)
          else if Ast.Expr.match_func header [ "signature" ] then (
            let signature_name =
              match FT.front header.elements with
              | None ->
                  Logger.error loc "Signature directives must declare a name";
                  exit 1
              | Some (_, signature_name) ->
                  Location.add_loc
                    (Ast.Expr.extract_functor_label signature_name)
                    signature_name.loc
            in
            match (arity, FT.front bodies) with
            | _, None ->
                Logger.error loc "Abstract signatures are not supported yet";
                (* TODO: implement abstract signatures *)
                exit 1
            | 1, Some (remaining, body) when FT.size remaining = 0 ->
                check_for_type_annotations body.content;
                let { module_ = nested; _ } =
                  preprocess_clauses { dependencies; filename } body.content
                in
                {
                  dependencies;
                  module_ =
                    {
                      module_ with
                      directives =
                        FT.snoc module_.directives
                        @@ Location.add_loc
                             (Ast.Module.Signature
                                {
                                  name = signature_name;
                                  body =
                                    {
                                      declarations =
                                        Ast.Module
                                        .multi_declaration_env_to_declaration_env
                                          nested.declarations;
                                      directives = nested.directives;
                                    };
                                })
                             loc;
                    };
                }
            | arity, Some _ when arity = 1 ->
                Logger.error loc
                @@ "Signatures can have at most one body and you have "
                ^ Int.to_string @@ FT.size bodies;
                FT.error_all_after 1 bodies "Extra body for signature here";
                exit 1
            | arity, _ ->
                Logger.error loc
                @@ "Signature directives must have exactly one atom as an \
                    argument and you have " ^ Int.to_string arity;
                FT.error_all_after 1 header.elements
                  "Extra signature argument here";
                exit 1)
          else
            match Config.preprocess_directive preprocess_clauses directive with
            | TargetSpecificDirective directive ->
                {
                  dependencies;
                  module_ =
                    {
                      module_ with
                      directives =
                        FT.snoc module_.directives
                        @@ Location.add_loc
                             (Ast.Module.TargetSpecific directive) loc;
                    };
                }
            | Update { dependencies = dependency_set; action } ->
                {
                  dependencies =
                    BatMap.String.add module_.name.content dependency_set
                      dependencies;
                  module_ =
                    {
                      module_ with
                      target_specific = action module_.target_specific;
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
               target_specific = Config.init_mods ();
               query = None;
             };
         }
end
