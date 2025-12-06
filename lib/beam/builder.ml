module Sys = Core.Erlang ()
open Sys
open Abstract_format

let lit l = Expr.Literal l
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

module Attribute = struct
  let module_ = Core.Form.module_form
  let file name line = Core.Form.FileAttr (name, line)
end

let rec pattern_list = function
  | [] -> pattern_nil
  | x :: xs -> pattern_cons x (pattern_list xs)

let clause = Clause.make
let simple_clause patterns body = clause patterns [] body
