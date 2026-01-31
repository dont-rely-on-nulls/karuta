module type EXPR = sig
  type t [@@deriving show]
  type base [@@deriving show]
  type func [@@deriving show]
  type call [@@deriving show]

  val extract_variable : t -> string
  val is_functor : t -> bool
end

module ClauseF (Expr : EXPR) = struct
  type head = { name : string; arity : int } [@@deriving show, ord]

  type multi_declaration = head * decl * decl Location.with_location list
  [@@deriving show]

  and base =
    | MultiDeclaration of multi_declaration
    | Query of { name : string; arity : int; args : string list }
    (* TODO: we want to allow multiple bodies for each directive. *)
    | Directive of Expr.func * t list
  [@@deriving show]

  and decl = {
    body : Expr.call Location.with_location list;
    original_arg_list : Expr.t list;
  }
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]
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

  (* TODO: we want to allow qualified atoms when declaring modules and signatures. *)
  type call =
    | Qualified of string Location.with_location * call
    | Unqualified of func Location.with_location
  [@@deriving show]

  let extract_variable : t -> string = function
    | { content = Variable name; _ } -> name
    | _ ->
        Logger.simply_error "Trying to extract a variable wrongly";
        exit 1

  let extract_functor_label : t -> string = function
    | { content = Functor { name; _ }; _ } -> name
    | _ ->
        Logger.simply_error "Trying to extract a variable wrongly";
        exit 1

  let is_functor : t -> bool = function
    | { content = Functor _; _ } -> true
    | _ -> false
end

module ParserClauseF (Expr : EXPR) = struct
  type base =
    | Declaration of decl
    | QueryConjunction of Expr.call Location.with_location list
    | Directive of Expr.func * t list
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]

  and decl = { head : Expr.func; body : Expr.call Location.with_location list }
  [@@deriving show]

  let is_decl : t -> bool = function
    | { content = Declaration _; _ } -> true
    | _ -> false
end

module Clause = ClauseF (Expr)
module ParserClause = ParserClauseF (Expr)
