{
open Parser
open Lexing

exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+

let digit = ['0'-'9']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let letter = lower_letter | upper_letter
let ident = lower_letter (letter | '_' | '-')*
let upper_ident = upper_letter (letter | '_')*
let int = '-'? ['0'-'9'] ['0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | newline { new_line lexbuf; read lexbuf }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | upper_ident { UPPER_IDENT (Lexing.lexeme lexbuf) }
  | int { INTEGER (Lexing.lexeme lexbuf) }
  | '?' { QUERY }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | ":-" { HOLDS }
  | ',' { COMMA }
  | '.' { DOT }
  | '[' { LEFT_DELIM }
  | ']' { RIGHT_DELIM }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
