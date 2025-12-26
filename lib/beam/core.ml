open Abstract_format

module type ERLANG = sig
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
      | Match of Pattern.t * t
      | Maybe of maybe list
      | MaybeElse of maybe list * Clause.clause list
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

    and maybe = Expr of t | MaybeBind of (Pattern.t * t)
    and association_type = Assoc | Exact

    and mapp =
      | Creation of (t * t) list
      | Update of t * (association_type * t * t) list

    and func =
      | Reference of (Primitives.atom * Primitives.arity)
      | ReferenceWithModule of
          (Primitives.atom * Primitives.atom * Primitives.arity)
      | Lambda of Clause.clause list
      | Named of (Primitives.variable * Clause.clause list)
      | Call of (t * t list)
      | CallWithModule of (t * t * t list)

    and comprehension = Bitstring of t | List of t | Map of t

    and bitstring = {
      value : t;
      size : t option;
      type_specifier : binary_type list;
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

    and association_type = Assoc | Exact

    and mapp =
      | Creation of (t * t) list
      | Update of t * (association_type * t * t) list

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
  end
end

module Erlang : ERLANG = struct
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
      | Match of Pattern.t * t
      | Maybe of maybe list
      | MaybeElse of maybe list * Clause.clause list
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

    and maybe = Expr of t | MaybeBind of (Pattern.t * t)
    and association_type = Assoc | Exact

    and mapp =
      | Creation of (t * t) list
      | Update of t * (association_type * t * t) list

    and func =
      | Reference of (Primitives.atom * Primitives.arity)
      | ReferenceWithModule of
          (Primitives.atom * Primitives.atom * Primitives.arity)
      | Lambda of Clause.clause list
      | Named of (Primitives.variable * Clause.clause list)
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
      | Match of Pattern.t * t
      | Maybe of maybe list
      | MaybeElse of maybe list * Clause.clause list
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

    and maybe = Expr of t | MaybeBind of (Pattern.t * t)
    and association_type = Assoc | Exact

    and mapp =
      | Creation of (t * t) list
      | Update of t * (association_type * t * t) list

    and func =
      | Reference of (Primitives.atom * Primitives.arity)
      | ReferenceWithModule of
          (Primitives.atom * Primitives.atom * Primitives.arity)
      | Lambda of Clause.clause list
      | Named of (Primitives.variable * Clause.clause list)
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

    and association_type = Assoc | Exact

    and mapp =
      | Creation of (t * t) list
      | Update of t * (association_type * t * t) list

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

    and association_type = Assoc | Exact

    and mapp =
      | Creation of (t * t) list
      | Update of t * (association_type * t * t) list

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

module type FORM = sig
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
    | ExportAttr of (Primitives.atom * Primitives.arity) list
    | ImportAttr of Primitives.atom * (Primitives.atom * int) list
    | ModuleAttr of Primitives.atom
    | FileAttr of string * int
    | Function of function_declaration
    | Specification of specification
    | Record of Primitives.atom * record_field list
    | Type of type_declaration
    | Wild
    | Eof of int

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

  val module_form : Primitives.atom -> t
  val export_form : (Primitives.atom * int) list -> t
  val function_form : Primitives.atom -> int -> Clause.clause list -> t
end

module Form (Erlang : ERLANG) :
  FORM
    with module Expr = Erlang.Expr
    with module Pattern = Erlang.Pattern
    with module Guard = Erlang.Guard
    with module Clause = Erlang.Clause = struct
  module Sys = Erlang
  module Expr = Sys.Expr
  module Pattern = Sys.Pattern
  module Guard = Sys.Guard
  module Clause = Sys.Clause

  type t =
    | ExportAttr of (Primitives.atom * Primitives.arity) list
    | ImportAttr of Primitives.atom * (Primitives.atom * int) list
    | ModuleAttr of Primitives.atom
    | FileAttr of string * int
    | Function of function_declaration
    | Specification of specification
    | Record of Primitives.atom * record_field list
    | Type of type_declaration
    | Wild
    | Eof of int

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

  let module_form name = ModuleAttr name
  let export_form exports = ExportAttr exports
  let function_form name arity clauses = Function { name; arity; clauses }
end
