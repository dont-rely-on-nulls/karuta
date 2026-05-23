open Compiler.Types
module Lookup = Database.Lookup
include Database.Types

module Clause = Ast.Clause (struct
  type extra_module_info = unit [@@deriving show]

  type 'declaration directive =
    | Persisted of 'declaration Location.with_location list
    | Ephemeral of 'declaration Location.with_location list
    | Constraint of 'declaration Location.with_location list
  [@@deriving show]
end)

let compile_clause ({ step; initialize_nested } : state Compiler.Types.runner)
    (clause : Clause.t) (compiler : state t) : state t =
  match clause.content with
  | Directive ({ content = header; loc }, body) ->
      Database.Directive.compile loc header body step compiler initialize_nested
  | MultiDeclaration _ ->
      Logger.error clause.loc
        "In Sakura, all declarations must be within a qualified Sakura \
         directive. Hence, no top-level declarations are allowed.";
      Logger.simply_error @@ "Available directives: "
      ^ Database.Types.formatted_supported_directives;
      exit 1
  | Query _ ->
      Logger.error clause.loc "Sakura's schemas do not allow queries";
      exit 1
