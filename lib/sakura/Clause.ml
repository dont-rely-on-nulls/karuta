open Compiler
include Types
module Lookup = Lookup

let preprocess_clauses = Preprocessor.preprocess_clauses

let compile_clause
    ({ step; initialize_nested } : (state, directives, mods) Compiler.runner)
    (clause : (directives, mods) Ast.Clause.t) (compiler : state t) : state t =
  match clause.content with
  | Directive directive ->
      Directive.compile clause.loc directive step compiler initialize_nested
  | MultiDeclaration _ ->
      Logger.error clause.loc
        "In Sakura, all declarations must be within a qualified Sakura \
         directive. Hence, no top-level declarations are allowed.";
      Logger.simply_error @@ "Available directives: "
      ^ Types.formatted_supported_directives;
      exit 1
  | Query _ ->
      Logger.error clause.loc "Sakura's schemas do not allow queries";
      exit 1
