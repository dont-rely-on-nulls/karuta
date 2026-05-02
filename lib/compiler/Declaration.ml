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
  | Functor ({ arity; _ } as f) when arity = 0 ->
      Builder.atom @@ Ast.Expr.extract_func_label f
  | Functor ({ elements; _ } as f) ->
      let name = Builder.atom @@ Ast.Expr.extract_func_label f in
      Builder.tuple (name :: List.map compile_expr elements)
  | Integer number -> Builder.int number

let call_with_fresh (name : string) expr =
  let open Beam in
  Ukanren.call_with_fresh @@ Builder.lambda name expr

let compile_declaration_bodies { module_name; imports; _ }
    (clauses : Ast.Clause.decl Location.with_location list) =
  if List.is_empty clauses then (
    Logger.simply_unreachable "Predicates must have at least one body";
    exit 1)
  else
    let open Beam in
    let compile_single_body
        ({ content; _ } : Ast.Clause.decl Location.with_location) :
        Builder.Expr.t =
      let find_variables call = Preprocessor.find_variables (Functor call) in
      let vars =
        content.body
        |> List.map (Fun.compose find_variables Location.strip_loc)
        |> List.fold_left Set.union Set.empty
        |> Set.filter (fun name ->
            Str.string_match (Str.regexp "^[A-Z]") name 0)
      in
      let open Location in
      let body =
        (* TODO: We should use locations when calling Beam helpers. They don't use
           locations yet, hence they are not being sent as arguments *)
        let make_function { content = call; loc } =
          let { Ast.Expr.name; elements; arity } = call in
          let args = List.map compile_expr elements in
          match (name, arity) with
          | ([ { content = "karuta"; _ } ], { content = "eq"; _ }), 2 -> (
              match args with
              | expr1 :: expr2 :: _ -> Ukanren.eq expr1 expr2
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'eq'";
                  exit 1)
          | ([ { content = "karuta"; _ } ], { content = "nat"; _ }), 1 -> (
              match args with
              | expr1 :: _ -> Ukanren.nat expr1
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'nat'";
                  exit 1)
          | ([], _), _ ->
              Builder.call
                (Builder.atom @@ Ast.Expr.extract_func_label call)
                args
          | ((head :: _ as path), { content = fun_name; _ }), _ ->
              Builder.call_with_module
                (Builder.atom
                @@ flat_module_name
                     (let suffix = List.map Location.strip_loc path in
                      if BatSet.String.mem head.content imports then suffix
                      else module_name :: suffix))
                (Builder.atom fun_name) args
        in

        content.body |> List.map make_function |> Ukanren.conj
      in
      Set.fold call_with_fresh vars body
    in
    clauses |> List.map compile_single_body |> Ukanren.disj

let compile_multi
    (({ name; arity }, first_clause, remaining_clauses) :
      Ast.Clause.head
      * Ast.Clause.decl Location.with_location
      * Ast.Clause.decl Location.with_location list)
    ({ env; _ } as compiler : t) : t =
  let declaration =
    let args =
      if arity = 0 then []
      else List.map string_of_int @@ BatList.range 0 `To (arity - 1)
    in
    Beam.Builder.single_function_declaration name
      (List.map (fun v -> Beam.Builder.Pattern.Variable v) args)
    @@ compile_declaration_bodies compiler (first_clause :: remaining_clauses)
  in
  let export = Beam.Builder.Attribute.export [ (name, arity) ] in
  {
    compiler with
    output = FT.cons (FT.snoc compiler.output declaration) export;
    env =
      {
        env with
        predicates = PredicateMap.add { name; arity } () env.predicates;
      };
  }
