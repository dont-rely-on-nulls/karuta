let parse filepath input =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filepath };
  try Parser.program Lexer.next_token lexbuf
  with Parser.Error ->
    let start_pos = !Lexer.error_start in
    let end_pos = !Lexer.error_end in
    Logger.error
      { startl = Location.to_t start_pos; endl = Location.to_t end_pos }
      "Syntax error";
    exit 1

let verify filepath : Ast.parser_clause list -> Ast.parser_clause list =
  function
  | [] ->
      print_endline @@ "Could not parse: " ^ filepath;
      []
  | clauses -> clauses
