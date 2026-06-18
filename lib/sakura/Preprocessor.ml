include Types
open Shared.Preprocessor

let is_sakura_file filepath = Filename.extension filepath = ".skr"

let renamer : Ast.ParserClause.decl -> Ast.Module.decl =
 fun _ -> failwith "TODO: Sakura renamer"

let validate_top_level (clause : Ast.ParserClause.t) : Ast.ParserClause.t =
  let open Ast.ParserClause in
  let open Location in
  let is_directive = function
    | { content = Declaration _; _ } -> false
    | { content = QueryConjunction _; _ } -> false
    | { content = Directive _; _ } -> true
  in
  if is_directive clause then clause
  else (
    Logger.error clause.loc "Found a non-directive in a Sakura file";
    exit 1)

let preprocess_clause (_group_clauses : (directives, mods) group)
    (_preprocessor : t) (_clauses : Ast.ParserClause.t) :
    (directives, mods) output =
  failwith "TODO: Sakura preprocess_clause"
