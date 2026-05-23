open Compiler.Types
module Lookup = Compiler.Lookup

module Clause = Ast.Clause (struct
  type extra_module_info = { imports : string Location.with_location BatSet.t }
  [@@deriving show]

  type 'declaration directive = | [@@deriving show]
end)

type state = unit

let initial_state () = ()

let compile_clause ({ step; initialize_nested } : unit Compiler.Types.runner)
    (clause : Ast.Clause.t) (compiler : unit t) : unit t =
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
          List.init (arity + 1) @@ Fun.const Builder.pattern_wildcard
        in
        args |> List.map Builder.var
        |> Builder.call (Builder.atom "")
        |> List.fold_right Ukanren.query_variable args
        |> List.fold_right Compiler.Declaration.call_with_fresh args
        |> Builder.single_function_declaration name fun_args
      in
      let export = Beam.Builder.Attribute.export [ (name, arity + 1) ] in
      {
        compiler with
        output = FT.cons (FT.snoc compiler.output declaration) export;
        env =
          {
            compiler.env with
            query =
              Some (Location.add_loc { Ast.Clause.name; arity } clause.loc);
          };
      }
