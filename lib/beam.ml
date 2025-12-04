module Primitives : sig
  type atom = string
  type var = string
  type literal =
    | LAtom of atom
    | LInteger of int
    | LString of string
    | LBitstring of string
    | LNil
  type unary_op =
    | UPlus | UMinus | UNot
  type binary_op =
    | BPlus | BMinus | BMult | BDiv | BRem
    | BSend
  val string_of_unary_op : unary_op -> string
  val string_of_binary_op : binary_op -> string
  val string_of_literal : literal -> string
end = struct
  type atom = string
  type var = string
  type literal =
    | LAtom of atom
    | LInteger of int
    | LString of string
    | LBitstring of string
    | LNil
  type unary_op =
    | UPlus | UMinus | UNot
  type binary_op =
    | BPlus | BMinus | BMult | BDiv | BRem
    | BSend
  let string_of_unary_op = function
    | UPlus -> "+"
    | UMinus -> "-"
    | UNot -> "not"
  let string_of_binary_op = function
    | BPlus -> "+"
    | BMinus -> "-"
    | BMult -> "*"
    | BDiv -> "/"
    | BRem -> "rem"
    | BSend -> "!"
  let string_of_literal = function
    | LAtom a -> a
    | LInteger i -> string_of_int i
    | LString s -> Printf.sprintf "\"%s\"" (String.escaped s)
    | LBitstring s -> Printf.sprintf "<<\"%s\">>" (String.escaped s)
end

module type EXPR = sig
  type t
  type pattern
  type guard
  type clause

  val literal : Primitives.literal -> t
  val var: Primitives.var -> t
  val tuple: t list -> t
  val binary: t
  val unary_op: Primitives.unary_op -> t -> t
  val binary_op: Primitives.binary_op -> t -> t -> t
  val block: t list -> t
end

module type PATTERN = sig
  type t
  type expr
  val literal : Primitives.literal -> t
  val var : Primitives.var -> t
  val wildcard : t
  val tuple : t list -> t
  val cons : t -> t -> t
  val list : t list -> t
  val binary : t
  val unary_op : Primitives.unary_op -> t -> t
  val binary_op : Primitives.binary_op -> t -> t -> t
  val record : Primitives.atom -> (Primitives.atom * t) list -> t
  val map : (t * t) list -> t
end

module type GUARD = sig
  type t
  type expr
  val expr : expr -> t
  val conj : t list -> t
  val disj : t list -> t
end

module type CLAUSE = sig
  type t
  type pattern
  type guard
  type expr

  val make: pattern list -> guard list -> expr list -> t
  val patterns: t -> pattern list
  val guards : t -> guard list
  val body: t -> expr list
end

module MakeExprSystem () = struct
  module rec Expr : sig
           type t =
             | ELiteral of Primitives.literal
             | EVar of Primitives.var
             | ETuple of t list
             | ECons of t * t
             | EBinary of binary_element list
             | EUnaryOp of Primitives.unary_op * t
             | EBinaryOp of Primitives.binary_op * t * t
             | ERecord of Primitives.atom * (Primitives.atom * t) list
             | ERecordAccess of t * Primitives.atom * Primitives.atom
             | ERecordUpdate of t * Primitives.atom * (Primitives.atom * t) list
             | EBlock of t list
           and binary_element =
             { value: t;
               size: t option;
               type_specifier: binary_type list }
           and binary_type =
             | BTInteger | BTBinary
         end = struct
               type t =
                 | ELiteral of Primitives.literal
                 | EVar of Primitives.var
                 | ETuple of t list
                 | ECons of t * t
                 | EBinary of binary_element list
                 | EUnaryOp of Primitives.unary_op * t
                 | EBinaryOp of Primitives.binary_op * t * t
                 | ERecord of Primitives.atom * (Primitives.atom * t) list
                 | ERecordAccess of t * Primitives.atom * Primitives.atom
                 | ERecordUpdate of t * Primitives.atom * (Primitives.atom * t) list
                 | EBlock of t list
               and binary_element =
                 { value: t;
                   size: t option;
                   type_specifier: binary_type list }
               and binary_type =
                 | BTInteger | BTBinary
             end
     and Pattern : sig
       type t =
         | PLiteral of Primitives.literal
         | PVar of Primitives.var
         | PWildcard
         | PTuple of t list
         | PCons of t * t
         | PBinary of binary_element list
         | PUnaryOp of Primitives.unary_op * t
         | PBinaryOp of Primitives.binary_op * t * t
         | PRecord of Primitives.atom * (Primitives.atom * t) list
         | PMap of (t*t) list
         | PMatch of t * t
       and binary_element =
         { value: t;
           size: Expr.t option;
           type_specifier: Expr.binary_type list }
     end = struct
           type t =
             | PLiteral of Primitives.literal
             | PVar of Primitives.var
             | PWildcard
             | PTuple of t list
             | PCons of t * t
             | PBinary of binary_element list
             | PUnaryOp of Primitives.unary_op * t
             | PBinaryOp of Primitives.binary_op * t * t
             | PRecord of Primitives.atom * (Primitives.atom * t) list
             | PMap of (t*t) list
             | PMatch of t * t
           and binary_element =
             { value: t;
               size: Expr.t option;
               type_specifier: Expr.binary_type list }
         end
  and Guard : sig
       type t =
         | GExpr of Expr.t
         | GConj of t list
         | GDisj of t list
     end = struct
           type t =
             | GExpr of Expr.t
             | GConj of t list
             | GDisj of t list
  end
  and Clause : sig
       type t =
         { patterns : Pattern.t list;
           guards : Guard.t list;
           body : Expr.t list;}
       val make : Pattern.t list -> Guard.t list -> Expr.t list -> t
       val patterns : t -> Pattern.t list
       val guards : t -> Guard.t list
       val body : t -> Expr.t list
     end = struct
           type t =
             { patterns : Pattern.t list;
               guards : Guard.t list;
               body : Expr.t list;}
           let make patterns guards body = {patterns; guards; body}
           let patterns {patterns; _} = patterns
           let guards {guards;_} = guards
           let body {body;_} = body
           
  end
end

(*Top level declaration trash*)
module Form : sig
  module Expr : sig type t end
  module Pattern : sig type t end
  module Guard : sig type t end
  module Clause : sig
    type t = { patterns: Pattern.t list; guards: Guard.t list; body: Expr.t list}
    val make : Pattern.t list -> Guard.t list -> Expr.t list -> t
  end
  type t =
    | FModule of Primitives.atom
    | FExport of (Primitives.atom * int) list
    | FImport of Primitives.atom * (Primitives.atom * int) list
    | FCompile of compile_option list
    | FFile of string * int
    | FFunction of function_declaration
    | FAttribute of Primitives.atom * Expr.t
    | FTypeDeclaration of type_declaration
    | FSpecification of specification
    | FRecord of Primitives.atom * record_field list
  and compile_option =
    | CExportAll
    | CInline of (Primitives.atom * int) list
    | COption of Primitives.atom
  and function_declaration =
    {name : Primitives.atom;
     arity: int;
     clauses : Clause.t list;}
  and type_declaration =
    {name: Primitives.atom;
     vars : Primitives.var list;
     type_definition: type_expr}
  and type_expr =
    | TAtom of Primitives.atom option
    | TInteger of int option
  and record_field =
    { field_name: Primitives.atom;
      field_type: type_expr option;
      default_value: Expr.t option}
  and specification =
    {name: Primitives.atom;
     arity : int;
     type_signatures: type_signatures list;}
  and type_signatures =
    { arg_types : type_expr list;
      return_type : type_expr;
      constraints: type_constraint list;}
  and type_constraint =
    {var : Primitives.var;
     subtype: type_expr}
  val module_form: Primitives.atom -> t
  val export_form: (Primitives.atom * int) list -> t
  val function_form: Primitives.atom -> int -> Clause.t list -> t
end = struct
  module Sys = MakeExprSystem()
  module Expr = Sys.Expr
  module Pattern = Sys.Pattern
  module Guard = Sys.Guard
  module Clause = Sys.Clause
  type t =
    | FModule of Primitives.atom
    | FExport of (Primitives.atom * int) list
    | FImport of Primitives.atom * (Primitives.atom * int) list
    | FCompile of compile_option list
    | FFile of string * int
    | FFunction of function_declaration
    | FAttribute of Primitives.atom * Expr.t
    | FTypeDeclaration of type_declaration
    | FSpecification of specification
    | FRecord of Primitives.atom * record_field list
  and compile_option =
    | CExportAll
    | CInline of (Primitives.atom * int) list
    | COption of Primitives.atom
  and function_declaration =
    {name : Primitives.atom;
     arity: int;
     clauses : Clause.t list;}
  and type_declaration =
    {name: Primitives.atom;
     vars : Primitives.var list;
     type_definition: type_expr}
  and type_expr =
    | TAtom of Primitives.atom option
    | TInteger of int option
  and record_field =
    { field_name: Primitives.atom;
      field_type: type_expr option;
      default_value: Expr.t option}
  and specification =
    {name: Primitives.atom;
     arity : int;
     type_signatures: type_signatures list;}
  and type_signatures =
    { arg_types : type_expr list;
      return_type : type_expr;
      constraints: type_constraint list;}
  and type_constraint =
    {var : Primitives.var;
     subtype: type_expr}
  let module_form name = FModule name
  let export_form exports = FExport exports
  let function_form name arity clauses = FFunction {name; arity; clauses;}
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
  module Sys = MakeExprSystem()
  open Sys

  let lit l = Sys.Expr.ELiteral l
  let var v = Sys.Expr.EVar v
  let atom a = Primitives.LAtom a
  let int i = Primitives.LInteger i
  let tuple ts = Sys.Expr.ETuple ts
  let cons h t = Sys.Expr.ECons (h, t)
  let nil = lit Primitives.LNil

  let rec list_expr = function
    | [] -> nil
    | x :: xs -> cons x (list_expr xs)

  let pattern_var v = Pattern.PVar v
  let pattern_wildcard = Pattern.PWildcard
  let pattern_literal l = Pattern.PLiteral l
  let pattern_integer i = Primitives.LInteger i
  let pattern_atom a = Primitives.LAtom a
  let pattern_tuple ts = Pattern.PTuple ts
  let pattern_cons h t = Pattern.PCons (h, t)
  let pattern_nil = pattern_literal Primitives.LNil
  let rec pattern_list = function
    | [] -> pattern_nil
    | x :: xs -> pattern_cons x (pattern_list xs)
  let clause = Clause.make
  let simple_clause patterns body = clause patterns [] body
end
