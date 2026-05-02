open Compiler.Types
module Lookup = Compiler.Lookup

let compile_clause ({ step; initialize_nested } : Compiler.Types.runner)
    (clause : Ast.Clause.t) (compiler : t) : t =
  (* TODO: handle location *)
  match clause.content with
  | Directive ({ loc; content = { name = _ :: _, _; _ } }, _) ->
      Logger.error loc "Directives cannot be qualified";
      exit 1
  | Directive ({ content = header; loc }, body) ->
      Shared.Directive.compile loc header body step compiler initialize_nested
  | MultiDeclaration (header, first, rest) ->
      let open Location in
      Compiler.Declaration.compile_multi
        (header, { content = first; loc = clause.loc }, rest)
        compiler
  | Query { name; arity; args } ->
      let open Beam in
      let declaration =
        let fun_args =
          List.init (arity + 1) @@ Fun.const Builder.pattern_wildcard
        in
        args |> List.map Builder.var
        |> Builder.call (Builder.atom "")
        |> List.fold_right Ukanren.query_variable args
        |> List.fold_right Compiler.Declaration.call_with_fresh args
        |> Ukanren.run_lazy
        |> Builder.single_function_declaration name fun_args
      in
      let export = Beam.Builder.Attribute.export [ (name, arity + 1) ] in
      {
        compiler with
        output = FT.cons (FT.snoc compiler.output declaration) export;
      }
