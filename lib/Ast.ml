module type EXPR = sig
  type t [@@deriving show]
  type base [@@deriving show]
  type func [@@deriving show]
  type func_label [@@deriving show]

  val extract_variable : t -> string
  val extract_unqualified_atom : t -> string
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
    | Directive of Expr.func Location.with_location * t list list
  [@@deriving show]

  and decl = {
    body : Expr.func Location.with_location list;
    original_arg_list : Expr.t list;
  }
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]
end

module Expr = struct
  type func_label =
    string Location.with_location list * string Location.with_location
  [@@deriving show]

  type func = { name : func_label; elements : t list; arity : int }
  [@@deriving show]

  and base =
    | Variable of string
    | Functor of func
    | Integer of int
    | Nil
    | Cons of t * t
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]

  let atom : func_label -> func = fun name -> { name; elements = []; arity = 0 }

  let func : func_label -> t list -> func =
   fun name elements -> { name; elements; arity = List.length elements }

  let func_label_of_string : string Location.with_location -> func_label =
   fun s -> ([], s)

  let functorr f = Functor f
  let integer i = Integer i
  let variable v = Variable v

  let extract_variable : t -> string = function
    | { content = Variable name; _ } -> name
    | _ ->
        Logger.simply_error "Trying to extract a variable wrongly";
        exit 1

  let extract_func_label : func -> string = function
    | { name = [], name; _ } -> name.content
    | { name = first_segment :: _, _; _ } ->
        Logger.simply_error first_segment.content;
        Logger.error first_segment.loc
          "Expected functor to have an unqualified label";
        exit 1

  let extract_functor_label : t -> string = function
    | { content = Functor f; _ } -> extract_func_label f
    | _ ->
        Logger.simply_error
          "Trying to extract a functor label out of a non-functor";
        exit 1

  let extract_unqualified_atom : t -> string = function
    | { content = Functor { name = [], unqualified_name; _ }; _ } ->
        unqualified_name.content
    | { content = Functor { name = _ :: _, _; _ }; loc } ->
        Logger.error loc "Expected unqualified atom (and it is qualified)";
        exit 1
    | { loc; _ } ->
        Logger.error loc "Expected unqualified atom";
        exit 1

  let is_functor : t -> bool = function
    | { content = Functor _; _ } -> true
    | _ -> false
end

module ParserClauseF (Expr : EXPR) = struct
  type base =
    | Declaration of decl
    | QueryConjunction of Expr.func Location.with_location list
    | Directive of Expr.func Location.with_location * t list list
  [@@deriving show]

  and t = base Location.with_location [@@deriving show]

  and decl = { head : Expr.func; body : Expr.func Location.with_location list }
  [@@deriving show]

  let query q = QueryConjunction q
  let declaration d = Declaration d
  let directive (header, bodies) = Directive (header, bodies)

  let is_decl : t -> bool = function
    | { content = Declaration _; _ } -> true
    | _ -> false
end

module Clause = ClauseF (Expr)
module ParserClause = ParserClauseF (Expr)
