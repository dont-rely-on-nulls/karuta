open Compiler.Types
module Lookup = Database.Lookup
include Database.Types

let compile_clause ({ step; initialize_nested } : state Compiler.Types.runner)
    (clause : Ast.Clause.t) (compiler : state t) : state t =
  match clause.content with
  | Directive ({ content = header; loc }, body) ->
      Database.Directive.compile loc header body step compiler initialize_nested
  | MultiDeclaration _ ->
      Logger.unreachable clause.loc
        "Step should never see declarations. Sakura has different declaration \
         types that cannot be compiled uniformly.";
      exit 1
  | Query _ ->
      Logger.error clause.loc "Sakura's schemas do not allow queries";
      exit 1
