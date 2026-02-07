open Lexing

type t = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
[@@deriving show, ord]

let to_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : t) :
    Lexing.position =
  { pos_fname; pos_lnum; pos_bol; pos_cnum }

let to_t ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) : t =
  { pos_fname; pos_lnum; pos_bol; pos_cnum }

type location = { startl : t; endl : t } [@@deriving show, ord]
type 'a with_location = { content : 'a; loc : location } [@@deriving show, ord]

let fmap f { content = a; loc } = { content = f a; loc }
let add p1 p2 v = { content = v; loc = { startl = to_t p1; endl = to_t p2 } }
let strip_loc (v : 'a with_location) : 'a = v.content
let add_loc (v : 'a) (loc : location) : 'a with_location = { content = v; loc }
