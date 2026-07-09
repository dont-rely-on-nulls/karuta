open Shared.Compiler

let compile :
    (Types.state, Types.directives, Types.mods) runner ->
    Types.state t ->
    (Types.directives, Types.mods) Ast.Module.directive Location.with_location ->
    Types.state t =
 fun runner
     ({ env = { modules = _; _ } as _env; state; _ } as compiler :
       Types.state t) { content = directive; loc = directive_loc } ->
  let module Lookup = (val compiler.lookup) in
  match directive with
  | Module _ -> Shared.Directive.compile directive_loc directive compiler runner
  | Signature _ ->
      Logger.error directive_loc
        "TODO: Sakura has special treatment for signatures";
      exit 1
  | TargetSpecific (Ephemeral _declarations) ->
      Logger.error directive_loc
        "TODO: ephemeral directive is not yet implemented";
      exit 1
  | TargetSpecific (Constraint _declarations) ->
      Logger.error directive_loc
        "TODO: constraint directive is not yet implemented";
      exit 1
  | TargetSpecific (Persisted declarations) ->
      BatMap.foldi Declaration.compile_persisted declarations compiler
