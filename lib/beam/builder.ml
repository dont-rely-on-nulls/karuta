open Abstract_format
open Core.Form (Core.Erlang)
module Expr = Expr
module Clause = Clause
module Pattern = Pattern
module Primitives = Primitives
module Guard = Guard

let lit l = Expr.Literal l
let var v = Expr.Variable v
let atom a = Expr.Literal (Primitives.Atom a)
let string a = Expr.Literal (Primitives.String a)
let int i = Expr.Literal (Primitives.Integer i)
let tuple ts = Expr.Tuple ts
let cons h t = Expr.Cons (h, t)
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
let call f args = Expr.Fun (Expr.Call (f, args))
let call_with_module m f args = Expr.Fun (Expr.CallWithModule (m, f, args))
let function_declaration name arity clauses = Function { name; arity; clauses }

let single_function_declaration name args expr =
  Function
    {
      name;
      arity = List.length args;
      clauses =
        [
          {
            guards = [];
            patterns = List.map (fun v -> Pattern.Variable v) args;
            body = [ expr ];
          };
        ];
    }

let lambda arg body =
  Expr.Lambda
    [ { body = [ body ]; guards = []; patterns = [ Pattern.Variable arg ] } ]

let rec pattern_list = function
  | [] -> pattern_nil
  | x :: xs -> pattern_cons x (pattern_list xs)

module Attribute = struct
  let module_ = module_form
  let export clauses = ExportAttr clauses
  let file name line = FileAttr (name, line)
  let eof line = Eof line
end

let clause = Clause.make
let simple_clause patterns body = clause patterns [] body
