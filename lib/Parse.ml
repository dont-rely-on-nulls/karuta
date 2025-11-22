let parse filepath input =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filepath };
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError (loc, message) ->
      Logger.error loc message;
      exit 1
  | Parser.Error ->
      (* TODO: nicer error messages *)
      Logger.error (Lexer.lexeme_position lexbuf) "Syntax Error";
      exit 1

let verify filepath : Ast.parser_clause list -> Ast.parser_clause list =
  function
  | [] ->
      print_endline @@ "Could not parse: " ^ filepath;
      []
  | clauses -> clauses
