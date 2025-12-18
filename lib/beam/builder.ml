open Abstract_format
open Core.Form (Core.Erlang)

let lit l = Expr.Literal l
let var v = Expr.Variable v
let atom a = Expr.Literal (Primitives.Atom a)
let string a = Expr.Literal (Primitives.String a)
let int i = Primitives.Integer i
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
let call_with_module m f args = Expr.Fun (Expr.CallWithModule (m, f, args))
let function_declaration name arity clauses = Function { name; arity; clauses }

let escape_atom (atom : string) =
  atom (* TODO: properly quote and escape the atom *)

let escape_string (string : string) =
  string (* TODO: properly quote and escape the string *)

let comma_separated_list f l = String.concat "," (List.map f l)

module Expression = struct
  let rec string_of_clause ({ body; _ } : Clause.clause) =
    (* TODO: patterns and guards *)
    "{clause,1,[],[],[" ^ comma_separated_list to_string body ^ "]}"

  and to_string (e : Expr.t) : string =
    (* TODO: line numbers shouldn't all be 1 *)
    match e with
    | Literal lit -> (
        match lit with
        | Atom name -> "{atom,1," ^ escape_atom name ^ "}"
        | Float f -> "{float,1," ^ string_of_float f ^ "}"
        | Integer i -> "{integer,1," ^ string_of_int i ^ "}"
        | String s -> "{string,1,\"" ^ escape_string s ^ "\"}"
        | Nil -> "{nil,1}")
    | Fun func -> (
        match func with
        | CallWithModule (module_name, function_name, args) ->
            "{call,1,{remote,1," ^ to_string module_name ^ ", "
            ^ to_string function_name ^ "},["
            ^ comma_separated_list to_string args
            ^ "]}"
        | Lambda clauses ->
            "{'fun',1,{clauses,["
            ^ comma_separated_list string_of_clause clauses
            ^ "]}}"
        | Reference (name, arity) ->
            "{'fun',1,{function," ^ escape_atom name ^ "," ^ string_of_int arity
            ^ "}}"
        | ReferenceWithModule (module_name, name, arity) ->
            "{'fun',1,{function,{atom,1," ^ escape_atom module_name
            ^ "},{atom,1" ^ escape_atom name ^ "}," ^ string_of_int arity ^ "}}"
        | Call (f, args) ->
            "{call,1," ^ to_string f ^ ",["
            ^ comma_separated_list to_string args
            ^ "]}"
        | Named (name, clauses) ->
            "{named_fun,1," ^ escape_atom name ^ ",["
            ^ comma_separated_list string_of_clause clauses
            ^ "]}")
    | _ -> failwith "TODO: Expression.to_string"
end

module Attribute = struct
  let module_ = module_form
  let export clauses = ExportAttr clauses
  let file name line = FileAttr (name, line)
  let eof line = Eof line

  let to_string (attr : t) : string =
    match attr with
    | ExportAttr exports ->
        "{attribute,1,export,["
        ^ comma_separated_list
            (fun (name, arity) ->
              "{" ^ escape_atom name ^ "," ^ string_of_int arity ^ "}")
            exports
        ^ "]}"
    | ModuleAttr name -> "{attribute,1,module," ^ escape_atom name ^ "}"
    | FileAttr (filename, line) ->
        "{attribute,1,file,{\"" ^ escape_string filename ^ ".krt\","
        ^ string_of_int line ^ "}}"
    | Eof line -> "{eof," ^ string_of_int line ^ "}"
    | Function { name; arity; clauses } ->
        "{function,1," ^ escape_atom name ^ "," ^ string_of_int arity ^ ",["
        ^ comma_separated_list Expression.string_of_clause clauses
        ^ "]}"
    | _ -> failwith "TODO"
end

let rec pattern_list = function
  | [] -> pattern_nil
  | x :: xs -> pattern_cons x (pattern_list xs)

let clause = Clause.make
let simple_clause patterns body = clause patterns [] body
