open Types

let compile _ _ _ = failwith "TODO"

(* let rec swap a f = *)
(*   let v = Atomic.get a in *)
(*   if Atomic.compare_and_set a v (f v) then () else swap a f *)

let compile_persisted ({ name; arity } : Ast.head)
    ({ content = { original_arg_list; _ }; _ } :
      Ast.Module.decl Location.with_location)
    ({ env; module_name; state; _ } as compiler : state Shared.Compiler.t) :
    state Shared.Compiler.t =
  let find_invalid_argument =
    FT.find_opt @@ fun arg ->
    Ast.Expr.is_underscore arg || (not @@ Ast.Expr.is_variable arg)
  in
  (match find_invalid_argument original_arg_list with
  | None -> ()
  | Some invalid ->
      Logger.error invalid.loc
      @@
      if Ast.Expr.is_underscore invalid then
        "Cannot use an underscore when defining an argument for a Sakura \
         persisted predicate"
      else "Every argument in a Sakura predicate definition must be a variable";
      exit 1);
  let full_name = BatString.lchop ~n:3 module_name ^ ":" ^ name in
  let declaration =
    let args =
      if arity = 0 then []
      else List.map string_of_int @@ BatList.range 0 `To (arity - 1)
    in
    Beam.Builder.single_function_declaration name
      (List.map (fun v -> Beam.Builder.Pattern.Variable v) args)
    @@ Beam.Builder.call_with_module
         (Beam.Builder.atom "sakura")
         (Beam.Builder.atom "ask")
         [
           Beam.Builder.map
             (original_arg_list |> FT.enum
             |> BatEnum.map
                  (Fun.compose Beam.Builder.atom Ast.Expr.extract_variable)
             |> BatList.of_enum)
             (List.map Beam.Builder.var args);
           Beam.Builder.list_expr
             (* TODO: Multi group namespace needs to be treated at some point *)
             [
               Beam.Builder.atom "Base";
               Beam.Builder.string @@ "sakura:" ^ full_name;
             ];
         ]
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
