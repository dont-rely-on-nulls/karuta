open Compiler
include Types
module Lookup = Lookup

let target_specific_directive _ _ _ = failwith "TODO"

let validate_top_level (clauses : Ast.ParserClause.t FT.t) :
    Ast.ParserClause.t FT.t =
  let open Ast.ParserClause in
  let open Location in
  let is_not_directive = function
    | { content = Declaration _; _ } -> true
    | { content = QueryConjunction _; _ } -> true
    | { content = Directive _; _ } -> false
  in
  match FT.find_opt is_not_directive clauses with
  | None -> clauses
  | Some { loc; _ } ->
      Logger.error loc "Found a non-directive in a Sakura file";
      exit 1

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
