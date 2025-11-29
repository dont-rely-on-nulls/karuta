{
open Parser
open Lexing

exception SyntaxError of (Location.location * string)

let lexeme_position (lexbuf): Location.location =
  let open Location in
  { startl = to_t @@ lexeme_start_p lexbuf; endl = to_t @@ lexeme_end_p lexbuf }

let syntax_error lexbuf message = raise (SyntaxError (lexeme_position lexbuf, message))

let read_atom lexbuf =
  let quoted_atom = (Lexing.lexeme lexbuf) in
  let atom = (String.sub quoted_atom 1 (String.length quoted_atom - 1)) in
  LITERAL_ATOM atom
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+

let digit = ['0'-'9']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let letter = lower_letter | upper_letter
let ident = lower_letter (letter | '_' | '-' | digit)*
let upper_ident = upper_letter (letter | '_' | digit)*
let int = '-'? ['0'-'9'] ['0'-'9']*
let quoted_atom = '\'' [^ '\'']* '\'' (* maybe we should disallow newlines in here *)

rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | upper_ident { UPPER_IDENT (Lexing.lexeme lexbuf) }
  | int { INTEGER (Lexing.lexeme lexbuf) }
  | '?' { QUERY }
  | quoted_atom { read_atom lexbuf }
  | ":-" { HOLDS }
  | ',' { COMMA }
  | '.' { DOT }
  | '[' { LEFT_DELIM }
  | ']' { RIGHT_DELIM }
  | '|' { PIPE }
  | "#%" { EXPRESSION_COMMENT }
  | '%' { skip_line lexbuf }
  | eof { EOF }
  | '\'' [^ '\'']* eof { syntax_error lexbuf "Quoted atom is not terminated." }
  | _ { syntax_error lexbuf ("Unexpected char: " ^ Lexing.lexeme lexbuf) }

and skip_line =
  parse
    | newline { new_line lexbuf; read lexbuf }
    | eof { EOF }
    | _ { skip_line lexbuf }
