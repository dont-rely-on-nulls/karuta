type tag = string [@@deriving show, ord]

type expr = Variable of var | Functor of func | Integer of int
[@@deriving show, ord]

and clause = MultiDeclaration of (decl * decl list) | Query of func
[@@deriving show, ord]

and parser_clause = Declaration of decl | QueryConjunction of func list
[@@deriving show, ord]

and var = { namev : tag } [@@deriving show, ord]

and func = { namef : tag; elements : expr list; arity : int }
[@@deriving show, ord]

and decl = { head : func; body : func list } [@@deriving show, ord]

type exprs = expr list [@@deriving show, ord]

module Clause = struct
  type t = clause [@@deriving show, ord]
end

module ParserClause = struct
  type t = parser_clause [@@deriving show, ord]
end

module Expr = struct
  type t = expr [@@deriving show, ord]
end
