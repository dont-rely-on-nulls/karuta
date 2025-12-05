module Primitives : sig
  type atom = string
  type variable = string
  type arity = int

  type literal =
    | Atom of atom
    | Character of char
    | Float of float
    | Integer of int
    | String of string
    | Nil

  type unary_op = Not

  type binary_op =
    | Plus
    | Minus
    | Mult
    | Div
    | Rem
    | IntDiv (* Arithmetic *)
    | Or
    | And
    | OrElse
    | AndAlso (* Logic *)
    | LEqual
    | Less
    | Greater
    | GEqual
    | Eq
    | Exact
    | NotExact (* Comparisons *)
    | Concat (* List Operations *)
    | Send (* Message Passing *)

  val string_of_unary_op : unary_op -> string
  val string_of_binary_op : binary_op -> string
  val string_of_literal : literal -> string
end = struct
  type atom = string
  type variable = string
  type arity = int

  type literal =
    | Atom of atom
    | Character of char
    | Float of float
    | Integer of int
    | String of string
    | Nil

  type unary_op = Not

  type binary_op =
    | Plus
    | Minus
    | Mult
    | Div
    | Rem
    | IntDiv
    | Or
    | And
    | OrElse
    | AndAlso
    | LEqual
    | Less
    | Greater
    | GEqual
    | Eq
    | Exact
    | NotExact
    | Concat
    | Send

  let string_of_unary_op = function Not -> "not"

  let string_of_binary_op = function
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Rem -> "rem"
    | Send -> "!"
    | IntDiv -> "div"
    | Or -> "or"
    | OrElse -> "orelse"
    | And -> "and"
    | AndAlso -> "andalso"
    | LEqual -> "=<"
    | Less -> "<"
    | Greater -> ">"
    | GEqual -> ">="
    | Eq -> "=="
    | Exact -> "=:="
    | NotExact -> "=/="
    | Concat -> "++"

  let string_of_literal = function
    | Atom a -> a
    | Character c -> "$" ^ String.make 1 c
    | Float f -> string_of_float f
    | Integer i -> string_of_int i
    | String s -> s
    | Nil -> "[]"
end

module type EXPR = sig
  type t
  type pattern
  type guard
  type clause

  val literal : Primitives.literal -> t
  val comprehension : t -> (pattern * t) list -> t
  val bitstring : t
  val block : t list -> t
  val case : t -> (pattern * guard option) list -> t
  val catch : t -> t
  val cons : t -> t -> t
  val fun_reference : Primitives.atom -> Primitives.arity -> t

  val fun_reference_with_module :
    Primitives.atom -> Primitives.atom -> Primitives.arity -> t

  val fun_lambda : clause list -> t
  val fun_named : Primitives.variable -> clause list -> t
  val function_call : t -> t list -> t
  val function_call_with_module : t -> t -> t list -> t
  val if_clause : (clause * guard option) list -> t
  val map_creation : (t * t) list -> t
  val map_update : t -> (t * t) list -> t
  val match_clause : pattern -> t
  val maybe_equals : pattern -> t -> t
  val maybe_alone : t list
  val maybe_else : t list -> (pattern * guard option) list -> t
  val nil : t
  val binary_op : t -> t -> Primitives.binary_op -> t
  val unary_op : t -> Primitives.unary_op -> t

  val receive :
    t ->
    (pattern * guard option) list ->
    (t * t) option ->
    t (* The tuple of t's is for the after *)

  val record_creation : Primitives.atom -> (Primitives.atom * t) list -> t
  val record_access : t -> Primitives.atom -> Primitives.atom -> t
  val record_update : t -> Primitives.atom -> (Primitives.atom * t) list -> t
  val tuple : t list -> t
  val try_clause : t -> (pattern * guard option) list -> t

  val try_with_patterns :
    t -> pattern list -> (pattern * guard option) list -> t

  val try_after : t -> (t * t) option -> t
  val try_after_with_patterns : t -> pattern list -> (t * t) option -> t

  val try_after_with_catch :
    t -> (t * t) option -> (pattern * guard option) list -> t

  val try_with_patterns_with_after_with_catch :
    t -> pattern list -> (pattern * guard option) list -> (t * t) option -> t

  val variable : Primitives.variable -> t
end

module type PATTERN = sig
  type t
  type expr

  val literal : Primitives.literal -> t
  val bitstring : (t * expr) list -> t
  val compound : t -> t -> t
  val cons : t -> t -> t
  val map_pattern : (t * t) list -> t
  val nil : t
  val binary_op : t -> Primitives.binary_op -> t -> t
  val unary_op : t -> Primitives.unary_op -> t -> t
  val record_pattern_named : Primitives.atom -> (Primitives.atom * t) list -> t
  val tuple : t list -> t
  val universal : t
  val variable : Primitives.atom -> t
end

module type GUARD = sig
  type t
  type expr

  val literal : Primitives.literal -> t
  val bitstring : (t * expr) list -> t
  val cons : t -> t -> t
  val function_call : Primitives.atom -> t list -> t

  val function_call_with_module :
    Primitives.atom -> Primitives.atom -> t list -> t

  val map_creation : (t * t) list -> t
  val map_update : t -> (t * t) list -> t
  val nil : t
  val binary_op : t -> Primitives.binary_op -> t -> t
  val unary_op : t -> Primitives.unary_op -> t -> t
  val record_creation : Primitives.atom -> (Primitives.atom * t) list -> t
  val record_access : t -> Primitives.atom -> Primitives.atom -> t
  val record_update : t -> Primitives.atom -> (Primitives.atom * t) list -> t
  val tuple : t list -> t
  val variable : Primitives.atom -> t
  val expr : expr -> t
  val conj : t list -> t
  val disj : t list -> t
end

module type CLAUSE = sig
  type t
  type pattern
  type guard
  type expr

  val case : pattern -> guard option -> expr -> t
  val catch : pattern -> guard option -> expr -> t
  val catch_with_atom : Primitives.atom -> pattern -> expr -> t

  val catch_with_atom_and_variable :
    Primitives.atom -> pattern -> Primitives.variable -> expr -> t

  val catch_with_guard : pattern -> guard -> expr -> t

  val catch_with_guard_and_atom :
    Primitives.atom -> pattern -> guard -> expr -> t

  val catch_with_guard_and_atom_and_variable :
    Primitives.atom -> pattern -> Primitives.variable -> guard -> expr -> t

  val function_clause : pattern -> expr -> t
  val function_clause_with_guard : pattern -> guard -> expr -> t
  val if_clause : guard -> expr -> t
  val make : pattern list -> guard list -> expr list -> t
  val patterns : t -> pattern list
  val guards : t -> guard list
  val body : t -> expr list
end
