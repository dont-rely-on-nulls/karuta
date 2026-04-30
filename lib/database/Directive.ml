open Compiler.Types

let compile (directive_loc : Location.location)
    ({ arity; _ } as func : Ast.Expr.func) (body : Ast.Clause.t list list)
    (step : Ast.Clause.t list * t -> t)
    ({ env = { modules = _; _ } as _env; _ } as compiler : t)
    (initialize_nested : Compiler.Types.initialize_nested) : t =
  let module Lookup = (val compiler.lookup) in
  match (Ast.Expr.extract_func_label func, arity, body) with
  | "module", _, _ ->
      Shared.Directive.compile directive_loc func body step compiler
        initialize_nested
  | "signature", _, _ ->
      Logger.error directive_loc
        "TODO: Sakura has special treatment for signatures";
      exit 1
  | "project", 0, _ ->
      Logger.error directive_loc "Sakura does not support project directive";
      exit 1
  | "persisted", _, _ ->
      Logger.error directive_loc
        "TODO: persisted directive is not yet implemented";
      exit 1
  | "ephemeral", _, _ ->
      Logger.error directive_loc
        "TODO: ephemeral directive is not yet implemented";
      exit 1
  | "constraint", _, _ ->
      Logger.error directive_loc
        "TODO: constraint directive is not yet implemented";
      exit 1
  | "stored", _, _ ->
      Logger.unreachable directive_loc
        "Stored procedures are not supported in Sakura yet";
      exit 1
  | _ ->
      Logger.simply_unreachable "Unknown directive for Sakura";
      exit 1
