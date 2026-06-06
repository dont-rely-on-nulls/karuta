include Types
module Lookup = Lookup

let target_specific_directive _ _ _ = failwith "TODO"
let validate_top_level = Fun.id

let compile_clause
    ({ step; initialize_nested } : (state, directives, mods) Compiler.runner)
    (clause : (directives, mods) Ast.Clause.t) (compiler : state Compiler.t) :
    state Compiler.t =
  (* TODO: handle location *)
  match clause.content with
  | Directive directive ->
      Directive.compile clause.loc directive step compiler initialize_nested
  | MultiDeclaration (header, first, rest) ->
      let open Location in
      Declaration.compile_multi
        (header, { content = first; loc = clause.loc }, rest)
        compiler
  | Query { name; args } ->
      let arity = FT.size args in
      (* TODO: undo the hack we did for the WAM with the fake declaration *)
      (match compiler.env.query with
      | None -> ()
      | Some { loc; _ } ->
          Logger.error clause.loc "A module can have at most one query";
          Logger.error loc "First query defined here";
          exit 1);
      let open Beam in
      let declaration =
        let fun_args =
          (* TODO: Builder does not use FT yet, hence we keep the list *)
          List.init (arity + 1) @@ Fun.const Builder.pattern_wildcard
        in
        let args = FT.to_list args in
        args |> List.map Builder.var
        |> Builder.call (Builder.atom "")
        |> List.fold_right Ukanren.query_variable args
        |> List.fold_right Declaration.call_with_fresh args
        |> Builder.single_function_declaration name fun_args
      in
      let export = Beam.Builder.Attribute.export [ (name, arity + 1) ] in
      {
        compiler with
        output = FT.cons (FT.snoc compiler.output declaration) export;
        env =
          {
            compiler.env with
            query = Some (Location.add_loc { Ast.name; arity } clause.loc);
          };
      }
