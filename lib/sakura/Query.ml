include Types

let compile (query : Ast.Module.query_ref Location.with_location option)
    compiler =
  match query with
  | None -> compiler
  | Some _ ->
      Logger.simply_unreachable
        "Compiling a query for Sakura means the its preprocessor is defective";
      exit 1
