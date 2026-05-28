let rename_arg ({ loc; _ } as expr : Ast.Expr.t) (counter : int) :
    Ast.Expr.func Location.with_location =
  let open Ast in
  let new_var = Location.add_loc (Expr.Variable (string_of_int counter)) loc in
  {
    content =
      {
        name = ([ { content = "karuta"; loc } ], { content = "eq"; loc });
        elements = [ new_var; expr ];
        arity = 2;
      };
    loc;
  }

let compare_clauses (c1 : Ast.ParserClause.t) (c2 : Ast.ParserClause.t) : int =
  match (c1, c2) with
  | ( { content = Declaration { head = h1; _ }; _ },
      { content = Declaration { head = h2; _ }; _ } ) ->
      Ast.Expr.compare_func h1 h2
  | _, _ -> -1

let decl_header ({ head = { arity; _ } as f; _ } : Ast.ParserClause.decl) :
    Ast.head =
  { name = Ast.Expr.extract_func_label f; arity }

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
       Expr.Functor
         { name = [ { content = "karuta"; _ } ], { content = "comment"; _ }; _ };
     _;
    } ->
        false
    | _ -> true
  in
  match clause with
  | {
   content =
     ParserClause.Directive
       ({ content = { name = [], { content = "comment"; _ }; _ }; _ }, _);
   _;
  } ->
      None
  | { content = ParserClause.Directive (head, body); loc } ->
      Some
        {
          content =
            ParserClause.Directive
              (head, List.map (List.filter_map remove_comments) body);
          loc;
        }
  | {
   content =
     Declaration { head = { name = [], { content = "comment"; _ }; _ }; _ };
   _;
  } ->
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
                  |> List.filter
                     @@ Fun.compose non_comment
                          (Location.fmap (fun v -> Ast.Expr.Functor v));
              };
        }
  | { content = QueryConjunction [ _ ]; _ } as query -> Some query
  | { content = QueryConjunction []; _ } -> None
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
              "The conjunctions we constructed are guaranteed not to have this \
               form.";
            exit 1)
      |> fun funcs ->
      if List.is_empty funcs then None
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
      List.fold_left
        (fun acc element -> S.union acc (find_variables @@ strip_loc element))
        S.empty more_elements
  | _ -> S.empty

module DependencyGraph = struct
  type t = BatSet.String.t BatMap.String.t

  let empty = BatMap.String.empty

  let merge =
    BatMap.String.merge (fun _ l r ->
        match (l, r) with
        | None, None -> None
        | v, None | None, v -> v
        | Some lset, Some rset -> Some (BatSet.String.union lset rset))

  let add key value graph =
    let open BatMap.String in
    match find_opt key graph with
    | None -> add key (BatSet.String.singleton value) graph
    | Some set -> add key (BatSet.String.add value set) graph

  let invert graph =
    let invert_one node children acc =
      BatSet.String.fold (fun child acc -> add child node acc) children acc
    in
    BatMap.String.fold invert_one graph BatMap.String.empty

  (* a <- b
     b <- c d
     d <- e *)

  (* Each node:
      * Append grandchildren to dependency set
      * Schedule a walk over grandchildren
      * Register that we already went over  *)

  type expansion_state = {
    forward : t;
    backward : t;
    visited : BatSet.String.t;
    next : string BatFingerTree.t;
  }

  type expansion_result = expansion_state Error.attempt

  (** Returns true if l depends on r in the given fully expanded graph, false
      otherwise *)
  let depends expanded_graph l r =
    match BatMap.String.find_opt l expanded_graph with
    | None -> false
    | Some deps -> BatSet.String.mem (ModuleName.of_filepath r) deps

  let no_cycles graph =
    (* TODO: explicitly say which files participate in each cycle instead
       of just saying each file depends on itself. *)
    let module Map = BatMap.String in
    if
      BatEnum.fold
        (fun has_cycle k ->
          if depends graph k k then (
            Logger.simply_error @@ "File " ^ k ^ " depends on itself";
            true)
          else has_cycle)
        false
      @@ Map.keys graph
    then exit 1;
    graph

  let expand graph : t Error.attempt =
    let module FT = BatFingerTree in
    let module Set = BatSet.String in
    let module Map = BatMap.String in
    let atom_to_filepath =
      Map.of_enum
      @@ BatEnum.map (fun k -> (ModuleName.of_filepath k, k))
      @@ Map.keys graph
    in
    let bridge_ancestor relatives node graph =
      if Set.is_empty relatives then graph
      else Map.modify_def Set.empty node (Set.union relatives) graph
    in
    let rec go (acc : expansion_result) : expansion_result =
      let open Error in
      let* { visited; forward; next; backward } = acc in
      match FT.front next with
      | Some (more, current) when Set.mem current visited ->
          go @@ ok { visited; forward; next = more; backward }
      | Some (more, current) -> (
          let visited = Set.add current visited in
          match
            Map.find_opt
              (Map.find_default current current atom_to_filepath)
              forward
          with
          | None -> go @@ ok { visited; forward; next = more; backward }
          | Some children ->
              let parents =
                Option.value ~default:Set.empty @@ Map.find_opt current backward
              in
              let forward =
                Set.fold (bridge_ancestor children) parents forward
              in
              let backward =
                Set.fold (bridge_ancestor parents) children backward
              in
              go
              @@ ok
                   {
                     visited;
                     forward;
                     next =
                       children |> Set.enum
                       |> BatEnum.filter (fun c -> not @@ Set.mem c visited)
                       |> BatEnum.fold FT.snoc more;
                     backward;
                   })
      | None -> Ok { visited; forward; next; backward }
    in
    Error.map (fun { forward; _ } -> no_cycles forward)
    @@ go
    @@ Error.ok
         {
           visited = BatSet.String.empty;
           forward = graph;
           backward = invert graph;
           next = BatFingerTree.of_enum (BatMap.String.keys graph);
         }

  let sort (expanded_graph : t) (files : string list) =
    let compare_files l r =
      if depends expanded_graph l r then 1
      else if depends expanded_graph r l then -1
      else 0
    in
    List.sort compare_files files
end

type t = { filename : string; dependencies : DependencyGraph.t }

let initialize filename : t = { filename; dependencies = BatMap.String.empty }

type ('directives, 'mods) output = {
  dependencies : DependencyGraph.t;
  clauses : ('directives, 'mods) Ast.Clause.t BatFingerTree.t;
}

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
    (Ast.ParserClause.t list -> ('directives, 'mods) output) ->
    Ast.ParserClause.t list list ->
    ('directives, 'mods) output =
 fun dependencies f grouped_clauses ->
  List.fold_left
    (fun acc elem -> merge acc @@ f elem)
    { dependencies; clauses = BatFingerTree.empty }
    grouped_clauses

module BatFingerTree = struct
  include BatFingerTree

  let sort f v =
    v |> BatFingerTree.to_list |> List.sort f |> BatFingerTree.of_list
end

module type TARGET = sig
  type directives
  type mods

  val target_specific_directive :
    Ast.Expr.func Location.with_location ->
    (directives, mods) Ast.Clause.t list list ->
    Location.location ->
    (directives, mods) Ast.Clause.directive
end

module type PREPROCESSOR = sig
  type directives
  type mods

  val preprocess : t -> Ast.ParserClause.t list -> (directives, mods) output
end

module Make (Target : TARGET) :
  PREPROCESSOR
    with type directives = Target.directives
    with type mods = Target.mods = struct
  include Target

  let rec parser_to_compiler :
      t -> Ast.ParserClause.t -> (directives, mods) output =
   fun ({ dependencies; filename } as preprocessor) clause ->
    let open Location in
    let open Ast in
    match clause with
    | { content = Directive (head, body); loc } ->
        let grouped_body = List.map (group_clauses preprocessor) body in
        let dependencies, grouped_clauses =
          List.fold_right
            (fun { dependencies; clauses } (deps_acc, clauses_acc) ->
              ( DependencyGraph.merge dependencies deps_acc,
                BatFingerTree.to_list clauses :: clauses_acc ))
            grouped_body (dependencies, [])
        in
        let dependencies =
          match
            (fst head.content.name, Ast.Expr.func_label head.content, body)
          with
          | [], "import", [] -> (
              match head.content.elements with
              | [ singleton ] ->
                  let external_dep =
                    Ast.Expr.extract_unqualified_atom singleton
                  in
                  DependencyGraph.add filename external_dep dependencies
              | [] ->
                  Logger.error head.loc
                    "Directive 'import' cannot be an empty functor";
                  exit 1
              | _ ->
                  Logger.error head.loc
                    "Directive 'import' cannot have multiple expressions within";
                  exit 1)
          | [], "import", _ ->
              Logger.error head.loc "Directive 'import' cannot have a body";
              exit 1
          | _ -> dependencies
        in
        {
          dependencies;
          clauses =
            BatFingerTree.of_list
              [
                {
                  content =
                    Ast.Clause.Directive
                      (target_specific_directive head grouped_clauses loc);
                  loc;
                };
              ];
        }
    | { content = Declaration decl; loc } ->
        {
          dependencies;
          clauses =
            BatFingerTree.of_list
              [
                {
                  content =
                    Ast.Clause.MultiDeclaration
                      (decl_header decl, rename_declaration decl, []);
                  loc;
                };
              ];
        }
    | { content = QueryConjunction calls; loc } ->
        let folder set call =
          S.union set (find_variables @@ Expr.Functor (strip_loc call))
        in
        let variables = List.fold_left folder S.empty calls in
        let list_variables = S.to_list variables in
        let query_name = "" in
        let head : Expr.func =
          {
            name = ([], { content = query_name; loc });
            elements =
              List.map
                (fun var -> { content = Expr.Variable var; loc })
                list_variables;
            arity = S.cardinal variables;
          }
        in
        let fake_decl : ParserClause.decl = { head; body = calls } in
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
        { dependencies; clauses = BatFingerTree.of_list [ declaration; query ] }

  and group_clauses ({ dependencies; _ } as preprocessor : t)
      (clauses : Ast.ParserClause.t list) =
    let multi_mapper (group : Ast.ParserClause.t list) =
      match group with
      | [ x ] -> parser_to_compiler preprocessor x
      | { content = Declaration first; loc } :: many ->
          {
            dependencies;
            clauses =
              BatFingerTree.of_list
                [
                  {
                    Location.content =
                      Ast.Clause.MultiDeclaration
                        ( decl_header first,
                          rename_declaration first,
                          List.map from_declaration many );
                    loc;
                  };
                ];
          }
      | _ ->
          Logger.simply_unreachable "unreachable group";
          exit 1
    in
    let open Batteries in
    clauses
    |> List.filter_map remove_comments
    |> List.map check_empty_heads |> List.group compare_clauses
    |> fold_map preprocessor.dependencies multi_mapper
  (* TODO *)

  let is_sakura_file filepath = Filename.extension filepath = ".skr"

  let validate_top_level (clauses : Ast.ParserClause.t list) :
      Ast.ParserClause.t list =
    let open Ast.ParserClause in
    let open Location in
    let is_not_directive = function
      | { content = Declaration _; _ } -> true
      | { content = QueryConjunction _; _ } -> true
      | { content = Directive _; _ } -> false
    in
    match List.find_opt is_not_directive clauses with
    | None -> clauses
    | Some { loc; _ } ->
        Logger.error loc "Found a non-directive in a Sakura file";
        exit 1

  let preprocess ({ filename; _ } as preprocessor : t) clauses =
    if is_sakura_file filename then
      validate_top_level clauses |> group_clauses preprocessor
    else group_clauses preprocessor clauses
end
