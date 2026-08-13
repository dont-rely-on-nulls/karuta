open Types

let flat_module_name (path : string list) =
  let concat_segments l r = l ^ ModuleName.separator ^ r in
  match path with
  | [] -> ""
  | head :: tail -> List.fold_left concat_segments head tail

let rec compile_expr (expr : Ast.Expr.t) : Beam.Builder.Expr.t =
  let open Beam in
  match Location.strip_loc expr with
  | Variable var -> Builder.var var
  | Nil -> Builder.nil
  | Cons (h, t) -> Builder.cons (compile_expr h) (compile_expr t)
  | Functor ({ elements; _ } as f) when FT.size elements = 0 ->
      Builder.atom @@ Ast.Expr.extract_func_label f
  | Functor ({ elements; _ } as f) ->
      let name = Builder.atom @@ Ast.Expr.extract_func_label f in
      (* TODO: use finger trees in Builder *)
      Builder.tuple @@ FT.to_list (FT.cons (FT.map compile_expr elements) name)
  | Integer number -> Builder.int number

let call_with_fresh (name : string) expr =
  let open Beam in
  Ukanren.call_with_fresh @@ Builder.lambda name expr

let compile_declaration_bodies
    ({ state; env; _ } as compiler : state Shared.Compiler.t)
    (clauses : Ast.Module.decl Location.with_location FT.t) =
  if FT.is_empty clauses then (
    Logger.simply_unreachable "Predicates must have at least one body";
    exit 1)
  else
    let open Beam in
    let compile_single_body
        ({ content; _ } : Ast.Module.decl Location.with_location) :
        Builder.Expr.t =
      let find_variables call =
        Shared.Preprocessor.find_variables (Functor call)
      in
      let vars =
        content.body
        |> FT.map (Fun.compose find_variables Location.strip_loc)
        |> FT.fold_left BatSet.union BatSet.empty
        |> BatSet.filter (fun name ->
            Str.string_match (Str.regexp "^[A-Z]") name 0)
      in
      let open Location in
      let body =
        (* TODO: We should use locations when calling Beam helpers. They don't use
           locations yet, hence they are not being sent as arguments *)
        let make_function { content = { Ast.Expr.name; elements } as call; loc }
            =
          match
            Lookup.predicate
              (Lookup.comptime_of_compiler compiler)
              compiler.env name (FT.size elements)
          with
          | `Undefined _ -> exit 1
          | `UnexpectedSignature loc ->
              Logger.error loc
                "Expected module name but found a signature instead";
              exit 1
          | `Ok { original_module } when original_module = env.qualifier ->
              let args = FT.map compile_expr elements in
              Builder.call (Builder.atom @@ Ast.Expr.extract_func_label call)
              @@ FT.to_list args
          | `Ok { original_module } ->
              let args = FT.map compile_expr elements in
              let _, { content = fun_name; _ } = name in
              Builder.call_with_module
                (original_module |> Shared.Compiler.ft_of_original_module
               |> FT.to_list |> flat_module_name |> Builder.atom)
                (Builder.atom fun_name) (FT.to_list args)
        in
        content.body |> FT.map make_function |> FT.to_list |> Ukanren.conj
      in
      BatSet.fold call_with_fresh vars body
    in
    clauses |> FT.map compile_single_body |> FT.to_list |> Ukanren.disj

let compile ({ name; arity } : Ast.head)
    ((first_clause, remaining_clauses) :
      Ast.Module.decl Location.with_location
      * Ast.Module.decl Location.with_location FT.t)
    ({ env; _ } as compiler : state Shared.Compiler.t) : state Shared.Compiler.t
    =
  let declaration =
    let args =
      if arity = 0 then []
      else List.map string_of_int @@ BatList.range 0 `To (arity - 1)
    in
    Beam.Builder.single_function_declaration name
      (List.map (fun v -> Beam.Builder.Pattern.Variable v) args)
    @@ compile_declaration_bodies compiler
         (FT.cons remaining_clauses first_clause)
  in
  let export = Beam.Builder.Attribute.export [ (name, arity) ] in
  {
    compiler with
    output = FT.cons (FT.snoc compiler.output declaration) export;
    env =
      {
        env with
        predicates =
          Shared.Compiler.PredicateMap.add { name; arity }
            Shared.Compiler.{ original_module = env.qualifier }
            env.predicates;
      };
  }
