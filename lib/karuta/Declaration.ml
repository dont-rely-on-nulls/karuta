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
    ({ module_name; state; _ } : state Shared.Compiler.t)
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
        let make_function { content = call; loc } =
          let { Ast.Expr.name; elements } = call in
          let arity = FT.size elements in
          let args = FT.map compile_expr elements in
          if Ast.Expr.match_func call [ "karuta"; "eq" ] && arity = 2 then (
            match FT.to_list args with
            | [ expr1; expr2 ] -> Ukanren.eq expr1 expr2
            | _ ->
                Logger.unreachable loc
                  "Mismatch between arity and length of elements in builtin \
                   'eq'";
                exit 1)
          else if Ast.Expr.match_func call [ "karuta"; "nat" ] && arity = 1 then (
            match FT.to_list args with
            | [ expr1 ] -> Ukanren.nat expr1
            | _ ->
                Logger.unreachable loc
                  "Mismatch between arity and length of elements in builtin \
                   'nat'";
                exit 1)
          else
            let path, { content = fun_name; _ } = name in
            match FT.head path with
            | None ->
                Builder.call (Builder.atom @@ Ast.Expr.extract_func_label call)
                @@ FT.to_list args
            | Some head ->
                Builder.call_with_module
                  (Builder.atom
                  @@ flat_module_name
                       (let suffix =
                          FT.to_list @@ FT.map Location.strip_loc path
                        in
                        if BatMap.String.mem head.content state.imports then
                          suffix
                        else module_name :: suffix))
                  (Builder.atom fun_name) (FT.to_list args)
        in

        content.body |> FT.map make_function |> FT.to_list |> Ukanren.conj
      in
      BatSet.fold call_with_fresh vars body
    in
    clauses |> FT.map compile_single_body |> FT.to_list |> Ukanren.disj

let compile_multi
    (({ name; arity }, first_clause, remaining_clauses) :
      Ast.head
      * Ast.Module.decl Location.with_location
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
          Shared.Compiler.PredicateMap.add { name; arity } () env.predicates;
      };
  }
