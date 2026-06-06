open Shared.Preprocessor

let is_sakura_file filepath = Filename.extension filepath = ".skr"

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

let preprocess_clauses (_preprocessor : t) (_clauses : Ast.ParserClause.t FT.t)
    : (Types.directives, Types.mods) output =
  failwith "TODO: Sakura"
