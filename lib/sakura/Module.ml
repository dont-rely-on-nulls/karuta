include Types
module Lookup = Lookup
module Preprocessor = Preprocessor

let compile_directive = Directive.compile
let compile_declaration = Declaration.compile

let compile_query query compiler =
  match query with
  | None -> compiler
  | Some { Location.loc; _ } ->
      Logger.error loc "Sakura's schemas do not allow queries";
      exit 1

(* let compile
    ({ step; initialize_nested } :
      (state, directives, mods) Shared.Compiler.runner)
    (module_ : (directives, mods) Ast.Module.t)
    (compiler : state Shared.Compiler.t) : state Shared.Compiler.t =
  match module_.content with
  | Directive directive ->
      Directive.compile module_.loc directive step compiler initialize_nested
  | MultiDeclaration _ ->
      Logger.error module_.loc
        "In Sakura, all declarations must be within a qualified Sakura \
         directive. Hence, no top-level declarations are allowed.";
      Logger.simply_error @@ "Available directives: "
      ^ Types.formatted_supported_directives;
      exit 1
  | Query _ ->
      Logger.error module_.loc "Sakura's schemas do not allow queries";
      exit 1
 *)
