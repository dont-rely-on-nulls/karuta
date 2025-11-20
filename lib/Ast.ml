module Location = struct
  include Lexing
  open Lexing

  type t = { pos_fname : string; pos_lnum : int; pos_bol : int; pos_cnum : int }
  [@@deriving show, ord]

  let to_position ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : t) :
      Lexing.position =
    { pos_fname; pos_lnum; pos_bol; pos_cnum }

  let to_t ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) : t =
    { pos_fname; pos_lnum; pos_bol; pos_cnum }

  type 'a with_location = { content : 'a; startl : t; endl : t }
  [@@deriving show, ord]

  let add p1 p2 v = { content = v; startl = to_t p1; endl = to_t p2 }
end

type tag = string [@@deriving show, ord]

type expr' = Variable of var | Functor of func | Integer of int
[@@deriving show, ord]

and expr = expr' Location.with_location [@@deriving show, ord]

and clause' = MultiDeclaration of (decl * decl list) | Query of func
[@@deriving show, ord]

and clause = clause' Location.with_location [@@deriving show, ord]

and parser_clause' = Declaration of decl | QueryConjunction of func list
[@@deriving show, ord]

and parser_clause = parser_clause' Location.with_location [@@deriving show, ord]
and var = { namev : tag } [@@deriving show, ord]

and func = { namef : tag; elements : expr list; arity : int }
[@@deriving show, ord]

and decl = { head : func; body : func list } [@@deriving show, ord]

type exprs' = expr list [@@deriving show, ord]
and exprs = exprs' Location.with_location [@@deriving show, ord]

module Clause = struct
  type t = clause [@@deriving show, ord]
end

module ParserClause = struct
  type t = parser_clause [@@deriving show, ord]

  let is_decl : t -> bool = function
    | { content = Declaration _; _ } -> true
    | _ -> false
end

module Expr = struct
  type t = expr [@@deriving show, ord]
end
