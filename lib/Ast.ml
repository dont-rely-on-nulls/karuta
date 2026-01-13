module type EXPR = sig
  type t [@@deriving show]
  type base [@@deriving show]
  type func [@@deriving show]

  val extract_variable : t -> string
end

module ClauseF (Expr : EXPR) = struct
  type base =
    | MultiDeclaration of (head * decl * decl Location.with_location list)
    | Query of { name : string; arity : int; args : string list }
  [@@deriving show]

  and decl = { body : Expr.func Location.with_location list } [@@deriving show]
  and head = { name : string; arity : int } [@@deriving show]

  type t = base Location.with_location [@@deriving show]
end

module Expr = struct
  type func = { name : string; elements : t list; arity : int }
  [@@deriving show]

  and base =
    | Variable of string
    | Functor of func
    | Integer of int
    | Nil
    | Cons of t * t
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]

  let extract_variable : t -> string = function
    | { content = Variable name; _ } -> name
    | _ ->
        Logger.simply_error "Trying to extract a variable wrongly";
        exit 1
end

module ParserClauseF (Expr : EXPR) = struct
  type base =
    | Declaration of decl
    | QueryConjunction of Expr.func Location.with_location list
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]

  and decl = { head : Expr.func; body : Expr.func Location.with_location list }
  [@@deriving show]

  let is_decl : t -> bool = function
    | { content = Declaration _; _ } -> true
    | _ -> false
end

module Clause = ClauseF (Expr)
module ParserClause = ParserClauseF (Expr)
