include Types

let compile query compiler =
  match query with
  | None -> compiler
  | Some _ ->
      Logger.simply_unreachable
        "Compiling a query for Sakura means the its preprocessor is defective";
      raise Exit
