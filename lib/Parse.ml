open Lexing
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

let env checkpoint =
  match checkpoint with I.HandlingError env -> env | _ -> assert false

let state checkpoint : int = I.current_state_number (env checkpoint)

let get checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      let open Location in
      { startl = Location.to_t pos1; endl = Location.to_t pos2 }
  | None ->
      failwith
        "The index is out of range. This should not happen if [$i]\n\
         keywords are correctly inside the syntax error message\n\
         database. The integer [i] should always be a valid offset\n\
         into the known suffix of the stack."

let fail buffer (checkpoint : _ I.checkpoint) =
  let location = L.range (E.last buffer) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message;
  exit 1

(* [attempt2 filename text] runs the parser. *)

let attempt2 filename text =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = L.init filename (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = UnitActionsParser.Incremental.main lexbuf.lex_curr_p in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let fail lexbuf (_ : Ast.parser_clause list I.checkpoint) =
  (* The parser has suspended itself because of a syntax error. Stop. *)
  (*
     match parser_state with
     | I.HandlingError _ ->
     ;
  *)
  Logger.error (Lexer.lexeme_position lexbuf) "Syntax Error";
  exit 1

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  I.loop_handle Fun.id (fail lexbuf) supplier result

let parse filepath input =
  let lexbuf = Lexing.from_string input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filepath };
  try loop lexbuf (Parser.Incremental.program lexbuf.lex_curr_p)
  with Lexer.SyntaxError (loc, message) ->
    Logger.error loc message;
    exit 1

let verify filepath : Ast.parser_clause list -> Ast.parser_clause list =
  function
  | [] ->
      print_endline @@ "Could not parse: " ^ filepath;
      []
  | clauses -> clauses
