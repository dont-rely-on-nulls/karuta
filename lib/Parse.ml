open Lexing
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module I = Parser.MenhirInterpreter

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
  let loc = { startl = to_t pos1; endl = to_t pos2 } in
  let msg = ParserMessages.message (state checkpoint) in
  let msg = E.expand (get input checkpoint) msg in
  Logger.error loc msg;
  exit 1

let parse_loop (input : string) (lexbuf : lexbuf) =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.read lexbuf in
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = Parser.Incremental.file lexbuf.lex_curr_p in
  I.loop_handle Fun.id (fail input buffer) supplier checkpoint

let parse (filepath : string) : Ast.ParserClause.t list =
  let input, lexbuf = L.read filepath in
  try parse_loop input lexbuf
  with Lexer.SyntaxError (loc, message) ->
    Logger.error loc message;
    exit 1
