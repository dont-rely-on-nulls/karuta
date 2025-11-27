open Lexing
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = UnitActionsParser.MenhirInterpreter

let env checkpoint =
  match checkpoint with
  | I.HandlingError env -> env
  | _ ->
      Logger.simply_unreachable "env: parser is broken";
      assert false

let state = Fun.compose I.current_state_number env

let get input checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      (pos1, pos2) |> E.extract input |> E.sanitize |> E.compress
  | None ->
      Logger.simply_unreachable "get: parser is broken";
      exit 1

let fail input buffer (checkpoint : _ I.checkpoint) =
  let pos1, pos2 = E.last buffer in
  let open Location in
  let loc = { startl = Location.to_t pos1; endl = Location.to_t pos2 } in
  let msg = ParserMessages.message (state checkpoint) in
  let msg = E.expand (get input checkpoint) msg in
  Logger.error loc msg;
  exit 1

let succeed _ =
  Logger.simply_unreachable "succeed: parser is broken";
  assert false

let next_parse filepath input =
  let lexbuf = L.init filepath (Lexing.from_string input) in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filepath };
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
  I.loop_handle succeed (fail input buffer) supplier checkpoint

let parse filepath =
  let input, lexbuf = L.read filepath in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = filepath };
  match Parser.program Lexer.read lexbuf with
  | parser_clauses -> parser_clauses
  | exception Lexer.SyntaxError (loc, message) ->
      Logger.error loc message;
      exit 1
  | exception Parser.Error -> next_parse filepath input

let verify filepath : Ast.parser_clause list -> Ast.parser_clause list =
  function
  | [] ->
      print_endline @@ "Could not parse: " ^ filepath;
      []
  | clauses -> clauses
