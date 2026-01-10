type tag = string [@@deriving show, ord]

type expr' =
  | Variable of var
  | Functor of func
  | Integer of int
  | Nil
  | Cons of expr * expr
[@@deriving show, ord]

and expr = expr' Location.with_location [@@deriving show, ord]

and clause' =
  | MultiDeclaration of
      (multi_decl_head * decl' * decl' Location.with_location list)
  | Query of func
[@@deriving show, ord]

and clause = clause' Location.with_location [@@deriving show, ord]

and parser_clause' =
  | Declaration of decl
  | QueryConjunction of func Location.with_location list
[@@deriving show, ord]

and parser_clause = parser_clause' Location.with_location [@@deriving show, ord]
and var = { namev : tag } [@@deriving show, ord]

and func = { namef : tag; elements : expr list; arity : int }
[@@deriving show, ord]

and decl = { head : func; body : func Location.with_location list }
[@@deriving show, ord]

and decl' = { body : func Location.with_location list } [@@deriving show, ord]
and multi_decl_head = { namem : tag; arity : int } [@@deriving show, ord]

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

  let extract_variable : t -> string = function
    | { content = Variable { namev }; _ } -> namev
    | _ ->
        Logger.simply_error "Trying to extract a variable wrongly";
        exit 1
end
