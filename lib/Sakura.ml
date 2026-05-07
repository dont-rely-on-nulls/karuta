open Compiler.Types
module Lookup = Database.Lookup
include Database.Types

let compile_clause ({ step; initialize_nested } : state Compiler.Types.runner)
    (clause : Ast.Clause.t) (compiler : state t) : state t =
  match clause.content with
  | Directive ({ loc; content = { name = _ :: _, _; _ } }, _) ->
      Logger.error loc "Directives cannot be qualified";
      exit 1
  | Directive ({ content = header; loc }, body) ->
      Database.Directive.compile loc header body step compiler initialize_nested
  | MultiDeclaration (_header, _first, _rest) -> compiler
  | Query _ ->
      Logger.error clause.loc "Sakura's schemas do not allow queries";
      exit 1
