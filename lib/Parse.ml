let parse filepath input =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filepath };
  try Parser.program Lexer.read lexbuf with
  | Lexer.SyntaxError (loc, message) ->
      Logger.error loc message;
      exit 1
  | Parser.Error ->
      Logger.error
        {
          startl =
            {
              (Location.to_t Lexing.dummy_pos) with
              Location.pos_fname = filepath;
            };
          endl =
            {
              (Location.to_t Lexing.dummy_pos) with
              Location.pos_fname = filepath;
            };
        }
        "FIXME: unhandled parser error";
      exit 1

let verify filepath : Ast.parser_clause list -> Ast.parser_clause list =
  function
  | [] ->
      print_endline @@ "Could not parse: " ^ filepath;
      []
  | clauses -> clauses
