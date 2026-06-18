include Types
module Lookup = Lookup
module Preprocessor = Preprocessor

let compile_directive = Directive.compile
let compile_declaration = Declaration.compile

let compile_query (query : Ast.Module.query_ref Location.with_location option)
    compiler =
  match query with
  | None -> compiler
  | Some { Location.loc; content = { name; args } } ->
      let arity = FT.size args in
      (* TODO: undo the hack we did for the WAM with the fake declaration *)
      (match compiler.env.query with
      | None -> ()
      | Some { loc; _ } ->
          Logger.error module_.loc "A module can have at most one query";
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
            query = Some (Location.add_loc { Ast.name; arity } module_.loc);
          };
      }
