module type EXPR = sig
  type t [@@deriving show]
  type base [@@deriving show]
  type func [@@deriving show]
  type func_label [@@deriving show]

  val extract_variable : t -> string
  val extract_unqualified_atom : t -> string
  val is_functor : t -> bool
end

module type DIRECTIVE = sig
  type 'declaration t [@@deriving show]
end

module ClauseF (Expr : EXPR) (Directives: DIRECTIVE) = struct
  type head = { name : string; arity : int } [@@deriving show, ord]

  type multi_declaration = head * decl * decl Location.with_location list
                                           [@@deriving show]

  and signature_ref =
    Named of string Location.with_location
  | Inlined of {declarations: multi_declaration Location.with_location list;
                directives: directive Location.with_location list}

  and sakura_directive =
    Persisted of multi_declaration Location.with_location list
  | Ephemeral of multi_declaration Location.with_location list
  | Constraint of multi_declaration Location.with_location list
  
  and directive =
    Module of {name: string Location.with_location;
               signature: signature_ref option;
               declarations: multi_declaration Location.with_location list;
               directives: directive Location.with_location list;
               imports: string Location.with_location BatSet.t}
  | Signature of {name: string Location.with_location;
                  declarations: multi_declaration Location.with_location list;
                  directives: directive Location.with_location list}
  | Sakura of sakura_directive

  and base =
    | MultiDeclaration of multi_declaration
    | Query of { name : string; arity : int; args : string list }
    | Directive of multi_declaration Directive.t
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

  let compare_func_label ((lhs_qualifier, lhs_name) : func_label)
      ((rhs_qualifier, rhs_name) : func_label) : int =
    let compare_segment =
     fun (lhs : string Location.with_location)
         (rhs : string Location.with_location) ->
      String.compare lhs.content rhs.content
    in
    match List.compare compare_segment lhs_qualifier rhs_qualifier with
    | 0 -> compare_segment lhs_name rhs_name
    | n -> n

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

  let compare_func lhs rhs : int =
    match Int.compare lhs.arity rhs.arity with
    | 0 -> compare_func_label lhs.name rhs.name
    | n -> n

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

  let extract_qualified_func_label :
      func -> string Location.with_location * string Location.with_location =
    function
    | { name = [], name; _ } ->
        Logger.error name.loc "Expected functor to have a qualified label";
        exit 1
    | { name = [ qualifier ], name; _ } -> (qualifier, name)
    | { name = qualifier :: _, _; _ } ->
        Logger.error qualifier.loc "Expected functor to have a single qualifier";
        exit 1

  let func_label : func -> string = function
    | { name = _, name; _ } -> name.content

  let extract_func_label : func -> string = function
    | { name = [], name; _ } -> name.content
    | { name = first_segment :: _, _; _ } ->
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
    | {
        content =
          Functor { name = [], unqualified_name; elements = []; arity = 0 };
        _;
      } ->
        unqualified_name.content
    | { content = Functor { name = [], _; _ }; loc } ->
        Logger.error loc "Expected unqualified atom and it has arguments";
        exit 1
    | { content = Functor { name = _ :: _, _; _ }; loc } ->
        Logger.error loc "Expected unqualified atom and it is qualified";
        exit 1
    | { loc; _ } ->
        Logger.error loc "Expected unqualified atom";
        exit 1

  let first_functor_atom_arg : t -> string =
   fun func_candidate ->
    match func_candidate with
    | { content = Functor { elements = []; _ }; loc } ->
        Logger.error loc
          "Tried to extract first functor atom argument from an atom";
        exit 1
    | { content = Functor { elements = first :: _; _ }; _ } ->
        extract_unqualified_atom first
    | _ ->
        Logger.error func_candidate.loc
          "Tried to extract first functor atom argument from a non Functor";
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
