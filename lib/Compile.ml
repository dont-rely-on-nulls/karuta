open Compiler.Types

(*
 * Strategy:
 *
 * A file is not necessarily a module.
 *
 * The build procedure must allow you to specify how the files relate
 * to the module system.
 *
 * By default, each file will be wrapped in a module, but the build system
 * may choose to concatenate multiple files into a single module.
 *)

let rec compile_clause (clause : Ast.Clause.t) (compiler : t) : t =
  (* TODO: handle location *)
  match clause.content with
  | Directive ({ loc; content = { name = _ :: _, _; _ } }, _) ->
      Logger.error loc "Directives cannot be qualified";
      exit 1
  | Directive ({ content = header; loc }, body) ->
      Compiler.Directive.compile loc header body step compiler
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

(* TODO: figure out the logistics of handling the runtime lib *)
and step : Ast.Clause.t list * t -> t = function
  | [], compiler ->
      compiler.persist compiler.filename
        (FT.append compiler.header compiler.output);
      compiler
  | clause :: remaining, compiler ->
      (* TODO: We should save the intermediary outputs, due to this being a step by module *)
      step (remaining, compile_clause clause compiler)
