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

module Erlang () = struct
  module rec Expr : sig
    type t =
      | Literal of Primitives.literal
      | Comprehension of comprehension
      | Bitstring of bitstring
      | Block of t list
      | Case of t
      | Catch of t
      | Cons of (t * t)
      | Fun of func
      | If of (Clause.t * Guard.t option)
      | Map of mapp
      | Match of Pattern.t
      | Maybe of maybe
      | Nil
      | BinaryOp of Primitives.binary_op * t * t
      | UnaryOp of Primitives.unary_op * t
      | Receive of receive
      | Record of record
      | Tuple of t list
      | Try of tryy
      | Variable of Primitives.variable

    and tryy =
      | Clause of t * (Pattern.t * Guard.t option) list
      | WithPatterns of {
          value : t;
          patterns : Pattern.t list;
          guards : (Pattern.t * Guard.t option) list;
        }
      | After of t * (t * t) option
      | AfterWithPatterns of {
          value : t;
          patterns : Pattern.t list;
          after : (t * t) option;
        }
      | AfterWithCatch of {
          value : t;
          guards : (Pattern.t * Guard.t option) list;
          after : (t * t) option;
        }
      | WithPatternsWithAfterWithCatch of {
          value : t;
          patterns : Pattern.t list;
          guards : (Pattern.t * Guard.t option) list;
          after : (t * t) option;
        }

    and record =
      | Creation of Primitives.atom * (Primitives.atom * t) list
      | Access of t * Primitives.atom * Primitives.atom
      | Update of t * Primitives.atom * (Primitives.atom * t) list

    and receive = {
      value : t;
      guards : (Pattern.t * Guard.t option) list;
      after : (t * t) option;
    }

    and maybe =
      | Equals of (Pattern.t * t)
      | Alone of t list
      | Else of (t list * (Pattern.t * Guard.t) option list)

    and mapp = Creation of (t * t) list | Update of t * (t * t) list

    and func =
      | Reference of (Primitives.atom * Primitives.arity)
      | ReferenceWithModule of
          (Primitives.atom * Primitives.atom * Primitives.arity)
      | Lambda of Clause.t list
      | Named of (Primitives.variable * Clause.t list)
      | Call of (t * t list)
      | CallWithModule of (t * t * t list)

    and comprehension = Bitstring of t | List of t | Map of t

    and bitstring = {
      value : t;
      size : t option;
      type_specifier : Expr.binary_type list;
    }

    and binary_type = Integer | Binary
  end = struct
    type t =
      | Literal of Primitives.literal
      | Comprehension of comprehension
      | Bitstring of bitstring
      | Block of t list
      | Case of t
      | Catch of t
      | Cons of (t * t)
      | Fun of func
      | If of (Clause.t * Guard.t option)
      | Map of mapp
      | Match of Pattern.t
      | Maybe of maybe
      | Nil
      | BinaryOp of Primitives.binary_op * t * t
      | UnaryOp of Primitives.unary_op * t
      | Receive of receive
      | Record of record
      | Tuple of t list
      | Try of tryy
      | Variable of Primitives.variable

    and tryy =
      | Clause of t * (Pattern.t * Guard.t option) list
      | WithPatterns of {
          value : t;
          patterns : Pattern.t list;
          guards : (Pattern.t * Guard.t option) list;
        }
      | After of t * (t * t) option
      | AfterWithPatterns of {
          value : t;
          patterns : Pattern.t list;
          after : (t * t) option;
        }
      | AfterWithCatch of {
          value : t;
          guards : (Pattern.t * Guard.t option) list;
          after : (t * t) option;
        }
      | WithPatternsWithAfterWithCatch of {
          value : t;
          patterns : Pattern.t list;
          guards : (Pattern.t * Guard.t option) list;
          after : (t * t) option;
        }

    and record =
      | Creation of Primitives.atom * (Primitives.atom * t) list
      | Access of t * Primitives.atom * Primitives.atom
      | Update of t * Primitives.atom * (Primitives.atom * t) list

    and receive = {
      value : t;
      guards : (Pattern.t * Guard.t option) list;
      after : (t * t) option;
    }

    and maybe =
      | Equals of (Pattern.t * t)
      | Alone of t list
      | Else of (t list * (Pattern.t * Guard.t) option list)

    and mapp = Creation of (t * t) list | Update of t * (t * t) list

    and func =
      | Reference of (Primitives.atom * Primitives.arity)
      | ReferenceWithModule of
          (Primitives.atom * Primitives.atom * Primitives.arity)
      | Lambda of Clause.t list
      | Named of (Primitives.variable * Clause.t list)
      | Call of (t * t list)
      | CallWithModule of (t * t * t list)

    and comprehension = Bitstring of t | List of t | Map of t

    and bitstring = {
      value : t;
      size : t option;
      type_specifier : Expr.binary_type list;
    }

    and binary_type = Integer | Binary
  end

  and Pattern : sig
    type t =
      | Literal of Primitives.literal
      | Bitstring of (t * bitstring) list
      | Compound of (t * t)
      | Cons of (t * t)
      | Map of (t * t) list
      | Nil
      | BinaryOp of Primitives.binary_op * t * t
      | UnaryOp of Primitives.unary_op * t
      | Record of (Primitives.atom * (Primitives.atom * t) list)
      | Tuple of t list
      | Universal
      | Variable of Primitives.variable
      | Wildcard

    and bitstring = {
      value : t;
      size : Expr.t option;
      type_specifier : Expr.binary_type list;
    }
  end = struct
    type t =
      | Literal of Primitives.literal
      | Bitstring of (t * bitstring) list
      | Compound of (t * t)
      | Cons of (t * t)
      | Map of (t * t) list
      | Nil
      | BinaryOp of Primitives.binary_op * t * t
      | UnaryOp of Primitives.unary_op * t
      | Record of (Primitives.atom * (Primitives.atom * t) list)
      | Tuple of t list
      | Universal
      | Variable of Primitives.variable
      | Wildcard

    and bitstring = {
      value : t;
      size : Expr.t option;
      type_specifier : Expr.binary_type list;
    }
  end

  and Guard : sig
    type t =
      | Literal of Primitives.literal
      | Bitstring of bitstring
      | Cons of t * t
      | Function of func
      | Map of mapp
      | Nil
      | BinaryOp of Primitives.binary_op * t * t
      | UnaryOp of Primitives.unary_op * t
      | Record of record
      | Tuple of t list
      | Variable of Primitives.variable
      | GExpr of Expr.t
      | GConj of t list
      | GDisj of t list

    and bitstring = {
      value : t;
      size : Expr.t option;
      type_specifier : Expr.binary_type list;
    }

    and func =
      | Call of Primitives.atom * t list
      | CallWithModule of {
          modl : Primitives.atom;
          name : Primitives.atom;
          args : t list;
        }

    and mapp = Creation of (t * t) list | Update of (t * (t * t) list)

    and record =
      | Creation of Primitives.atom * (Primitives.atom * t) list
      | Access of t * Primitives.atom * Primitives.atom
      | Update of t * Primitives.atom * (Primitives.atom * t) list
  end = struct
    type t =
      | Literal of Primitives.literal
      | Bitstring of bitstring
      | Cons of t * t
      | Function of func
      | Map of mapp
      | Nil
      | BinaryOp of Primitives.binary_op * t * t
      | UnaryOp of Primitives.unary_op * t
      | Record of record
      | Tuple of t list
      | Variable of Primitives.variable
      | GExpr of Expr.t
      | GConj of t list
      | GDisj of t list

    and bitstring = {
      value : t;
      size : Expr.t option;
      type_specifier : Expr.binary_type list;
    }

    and func =
      | Call of Primitives.atom * t list
      | CallWithModule of {
          modl : Primitives.atom;
          name : Primitives.atom;
          args : t list;
        }

    and mapp = Creation of (t * t) list | Update of (t * (t * t) list)

    and record =
      | Creation of Primitives.atom * (Primitives.atom * t) list
      | Access of t * Primitives.atom * Primitives.atom
      | Update of t * Primitives.atom * (Primitives.atom * t) list
  end

  and Clause : sig
    type t =
      | Case of Pattern.t * Guard.t option
      | Catch of catchh
      | Function of func
      | If of Guard.t * Expr.t

    and catchh =
      | Default of Pattern.t * Guard.t option
      | WithAtom of Primitives.atom * Pattern.t
      | WithAtomAndVariable of {
          name : Primitives.atom;
          pattern : Pattern.t;
          variable : Primitives.variable;
        }
      | WithGuard of { pattern : Pattern.t; guard : Guard.t }
      | WithGuardAndAtom of {
          name : Primitives.atom;
          pattern : Pattern.t;
          guard : Guard.t;
          expr : Expr.t;
        }
      | WithGuardAndAtomAndVariable of {
          name : Primitives.atom;
          pattern : Pattern.t;
          variable : Primitives.variable;
          guard : Guard.t;
          expr : Expr.t;
        }

    and func =
      | Default of (Pattern.t * Expr.t)
      | WithGuard of { pattern : Pattern.t; guard : Guard.t; expr : Expr.t }

    and clause = {
      patterns : Pattern.t list;
      guards : Guard.t list;
      body : Expr.t list;
    }

    val make : Pattern.t list -> Guard.t list -> Expr.t list -> clause
    val patterns : clause -> Pattern.t list
    val guards : clause -> Guard.t list
    val body : clause -> Expr.t list
  end = struct
    type t =
      | Case of Pattern.t * Guard.t option
      | Catch of catchh
      | Function of func
      | If of Guard.t * Expr.t

    and catchh =
      | Default of Pattern.t * Guard.t option
      | WithAtom of Primitives.atom * Pattern.t
      | WithAtomAndVariable of {
          name : Primitives.atom;
          pattern : Pattern.t;
          variable : Primitives.variable;
        }
      | WithGuard of { pattern : Pattern.t; guard : Guard.t }
      | WithGuardAndAtom of {
          name : Primitives.atom;
          pattern : Pattern.t;
          guard : Guard.t;
          expr : Expr.t;
        }
      | WithGuardAndAtomAndVariable of {
          name : Primitives.atom;
          pattern : Pattern.t;
          variable : Primitives.variable;
          guard : Guard.t;
          expr : Expr.t;
        }

    and func =
      | Default of (Pattern.t * Expr.t)
      | WithGuard of { pattern : Pattern.t; guard : Guard.t; expr : Expr.t }

    and clause = {
      patterns : Pattern.t list;
      guards : Guard.t list;
      body : Expr.t list;
    }

    let make patterns guards body = { patterns; guards; body }
    let patterns { patterns; _ } = patterns
    let guards { guards; _ } = guards
    let body { body; _ } = body
  end
end

module Form : sig
  module Expr : sig
    type t
  end

  module Pattern : sig
    type t
  end

  module Guard : sig
    type t
  end

  module Clause : sig
    type clause = {
      patterns : Pattern.t list;
      guards : Guard.t list;
      body : Expr.t list;
    }

    val make : Pattern.t list -> Guard.t list -> Expr.t list -> clause
  end

  type t =
    | Module of Primitives.atom * t list
    | ExportAttr of (Primitives.atom * int) list
    | ImportAttr of Primitives.atom * (Primitives.atom * int) list
    | ModuleAttr of Primitives.atom
    | FileAttr of string * int
    | Function of function_declaration
    | Specification of specification
    | Record of Primitives.atom * record_field list
    | Type of type_declaration
    | Wild

  and compile_option =
    | ExportAll
    | Inline of (Primitives.atom * int) list
    | Option of Primitives.atom

  and function_declaration = {
    name : Primitives.atom;
    arity : Primitives.arity;
    clauses : Clause.clause list;
  }

  and type_declaration = {
    name : Primitives.atom;
    vars : Primitives.variable list;
    type_definition : type_expr;
  }

  and type_expr = Atom of Primitives.atom option | Integer of int option

  and record_field = {
    field_name : Primitives.atom;
    field_type : type_expr option;
    default_value : Expr.t option;
  }

  and specification =
    | Default of {
        name : Primitives.atom;
        arity : Primitives.arity;
        type_signatures : type_signature list;
      }
    | WithModule of {
        modl : Primitives.atom;
        name : Primitives.atom;
        arity : Primitives.arity;
        type_signatures : type_signature list;
      }

  and type_signature = {
    arg_types : type_expr list;
    return_type : type_expr;
    constraints : type_constraint list;
  }

  and type_constraint = { var : Primitives.variable; subtype : type_expr }

  val module_form : Primitives.atom -> t list -> t
  val export_form : (Primitives.atom * int) list -> t
  val function_form : Primitives.atom -> int -> Clause.clause list -> t
end = struct
  module Sys = Erlang ()
  module Expr = Sys.Expr
  module Pattern = Sys.Pattern
  module Guard = Sys.Guard
  module Clause = Sys.Clause

  type t =
    | Module of Primitives.atom * t list
    | ExportAttr of (Primitives.atom * int) list
    | ImportAttr of Primitives.atom * (Primitives.atom * int) list
    | ModuleAttr of Primitives.atom
    | FileAttr of string * int
    | Function of function_declaration
    | Specification of specification
    | Record of Primitives.atom * record_field list
    | Type of type_declaration
    | Wild

  and compile_option =
    | ExportAll
    | Inline of (Primitives.atom * int) list
    | Option of Primitives.atom

  and function_declaration = {
    name : Primitives.atom;
    arity : Primitives.arity;
    clauses : Clause.clause list;
  }

  and type_declaration = {
    name : Primitives.atom;
    vars : Primitives.variable list;
    type_definition : type_expr;
  }

  and type_expr = Atom of Primitives.atom option | Integer of int option

  and record_field = {
    field_name : Primitives.atom;
    field_type : type_expr option;
    default_value : Expr.t option;
  }

  and specification =
    | Default of {
        name : Primitives.atom;
        arity : Primitives.arity;
        type_signatures : type_signature list;
      }
    | WithModule of {
        modl : Primitives.atom;
        name : Primitives.atom;
        arity : Primitives.arity;
        type_signatures : type_signature list;
      }

  and type_signature = {
    arg_types : type_expr list;
    return_type : type_expr;
    constraints : type_constraint list;
  }

  and type_constraint = { var : Primitives.variable; subtype : type_expr }

  let module_form name forms = Module (name, forms)
  let export_form exports = ExportAttr exports
  let function_form name arity clauses = Function { name; arity; clauses }
end

(* module type AST = sig *)
(*   module Expr : sig *)
(*     type t *)
(*     include EXPR with type t := t *)
(*   end *)
(*   module Pattern : PATTERN *)
(*   module Guard: GUARD *)
(*   module Clause : CLAUSE *)
(*   module Form : sig *)
(*     type t *)
(*     val forms: t -> Form.t list *)
(*   end *)
(* end *)

module Builder = struct
  module Sys = Erlang ()
  open Sys

  let lit l = Sys.Expr.Literal l
  let var v = Sys.Expr.Variable v
  let atom a = Primitives.Atom a
  let int i = Primitives.Integer i
  let tuple ts = Sys.Expr.Tuple ts
  let cons h t = Sys.Expr.Cons (h, t)
  let nil = lit Primitives.Nil
  let rec list_expr = function [] -> nil | x :: xs -> cons x (list_expr xs)
  let pattern_var v = Pattern.Variable v
  let pattern_wildcard = Pattern.Wildcard
  let pattern_literal l = Pattern.Literal l
  let pattern_integer i = Primitives.Integer i
  let pattern_atom a = Primitives.Atom a
  let pattern_tuple ts = Pattern.Tuple ts
  let pattern_cons h t = Pattern.Cons (h, t)
  let pattern_nil = pattern_literal Primitives.Nil

  let rec pattern_list = function
    | [] -> pattern_nil
    | x :: xs -> pattern_cons x (pattern_list xs)

  let clause = Clause.make
  let simple_clause patterns body = clause patterns [] body
end
