open Compiler.Types

module Error = struct
  module Set = BatSet.String

  let supported_directives =
    Set.of_list [ "persisted"; "ephemeral"; "constraint"; "stored" ]

  let treat_error_cases
      ((qualifier, name) :
        string Location.with_location * string Location.with_location)
      (arity : int) : unit =
    if arity <> 0 then (
      Logger.error qualifier.loc "Sakura directives should be qualified atoms";
      exit 1)
    else if qualifier.content = "sakura" then (
      if Set.mem name.content supported_directives then ()
      else Logger.error name.loc @@ "Undefined Sakura directive:" ^ name.content;
      exit 1)
    else (
      Logger.error qualifier.loc
        "Sakura directives should be qualified with 'sakura'";
      exit 1)
end

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
  | _ -> (
      let open Location in
      let ((qualifier, name) as func_label) =
        Ast.Expr.extract_qualified_func_label func
      in
      match (qualifier.content, name.content, arity) with
      | "sakura", "persisted", 0 ->
          Logger.error directive_loc
            "TODO: persisted directive is not yet implemented";
          exit 1
      | "sakura", "ephemeral", 0 ->
          Logger.error directive_loc
            "TODO: ephemeral directive is not yet implemented";
          exit 1
      | "sakura", "constraint", 0 ->
          Logger.error directive_loc
            "TODO: constraint directive is not yet implemented";
          exit 1
      | "sakura", "stored", 0 ->
          Logger.unreachable directive_loc
            "Stored procedures are not supported in Sakura yet";
          exit 1
      | _ ->
          Error.treat_error_cases func_label arity;
          Logger.simply_unreachable "Unknown directive for Sakura";
          exit 1)
