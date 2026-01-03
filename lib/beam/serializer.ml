open Abstract_format
open Core.Form (Core.Erlang)
module Expr = Expr
module Clause = Clause
module Pattern = Pattern
module Guard = Guard

let escape_atom (atom : string) =
  "'" ^ atom ^ "'" (* TODO: properly escape the atom *)

let escape_string (string : string) =
  "\"" ^ string ^ "\"" (* TODO: properly escape the string *)

let comma_separated_list f l = "[" ^ String.concat "," (List.map f l) ^ "]"

module Expression = struct
  (* TODO: line numbers shouldn't all be 1 *)
  let string_of_tuple (string_of_content : 'a -> string) (contents : 'a list) =
    "{tuple,1," ^ comma_separated_list string_of_content contents ^ "}"

  let rec string_of_clause ({ body; patterns; guards } : Clause.clause) =
    "{clause,1,"
    ^ comma_separated_list string_of_pattern patterns
    ^ ","
    ^ comma_separated_list string_of_guard guards
    ^ ","
    ^ comma_separated_list to_string body
    ^ "}"

  and string_of_pattern (pattern : Pattern.t) =
    match pattern with
    | Literal lit -> string_of_literal lit
    | Variable name -> string_of_var name
    | Tuple contents -> string_of_tuple string_of_pattern contents
    | Bitstring _ | Compound _ | Cons _ | Map _ | BinaryOp _ | UnaryOp _
    | Record _ | Universal | Wildcard ->
        "TODO"

  and string_of_literal (lit : Primitives.literal) =
    match lit with
    | Atom name -> "{atom,1," ^ escape_atom name ^ "}"
    | Float f -> "{float,1," ^ string_of_float f ^ "}"
    | Integer i -> "{integer,1," ^ string_of_int i ^ "}"
    | String s -> "{string,1," ^ escape_string s ^ "}"
    | Nil -> "{nil,1}"

  and string_of_guard (guard : Guard.t) =
    match guard with
    | Literal lit -> string_of_literal lit
    | Variable name -> string_of_var name
    | Tuple contents -> string_of_tuple string_of_guard contents
    | Bitstring _ | Cons _ | Function _ | Map _ | BinaryOp _ | UnaryOp _
    | Record _ | GExpr _ | GConj _ | GDisj _ ->
        "TODO"

  and string_of_var (name : string) = "{var,1," ^ escape_atom name ^ "}"

  and string_of_maybe maybe_expr =
    let open Expr in
    match maybe_expr with
    | MaybeBind (pattern, expr) ->
        "{maybe_match,1," ^ string_of_pattern pattern ^ "," ^ to_string expr
        ^ "}"
    | Expr expr -> to_string expr

  and to_string (e : Expr.t) : string =
    match e with
    | Literal lit -> string_of_literal lit
    | Fun func -> (
        match func with
        | CallWithModule (module_name, function_name, args) ->
            "{call,1,{remote,1," ^ to_string module_name ^ ", "
            ^ to_string function_name ^ "},"
            ^ comma_separated_list to_string args
            ^ "}"
        | Lambda clauses ->
            "{'fun',1,{clauses,"
            ^ comma_separated_list string_of_clause clauses
            ^ "}}"
        | Reference (name, arity) ->
            "{'fun',1,{function," ^ escape_atom name ^ "," ^ string_of_int arity
            ^ "}}"
        | ReferenceWithModule (module_name, name, arity) ->
            "{'fun',1,{function,{atom,1," ^ escape_atom module_name
            ^ "},{atom,1" ^ escape_atom name ^ "}," ^ string_of_int arity ^ "}}"
        | Call (f, args) ->
            "{call,1," ^ to_string f ^ ","
            ^ comma_separated_list to_string args
            ^ "}"
        | Named (name, clauses) ->
            "{named_fun,1," ^ escape_atom name ^ ","
            ^ comma_separated_list string_of_clause clauses
            ^ "}")
    | Tuple contents -> string_of_tuple to_string contents
    | Map (Creation entries) ->
        "{map,1,"
        ^ comma_separated_list
            (fun (k, v) ->
              "{map_field_assoc,1," ^ to_string k ^ "," ^ to_string v ^ "}")
            entries
        ^ "}"
    | Map (Update (base, entries)) ->
        "{map,1," ^ to_string base ^ ","
        ^ comma_separated_list
            (fun (tag, k, v) ->
              "{map_field_"
              ^ (match tag with Expr.Exact -> "exact" | Expr.Assoc -> "assoc")
              ^ ",1," ^ to_string k ^ "," ^ to_string v ^ "}")
            entries
        ^ "}"
    | Cons (lhs, rhs) -> "{cons,1," ^ to_string lhs ^ "," ^ to_string rhs ^ "}"
    | BinaryOp (op, lhs, rhs) ->
        "{op,1,'"
        ^ Primitives.string_of_binary_op op
        ^ "', " ^ to_string lhs ^ "," ^ to_string rhs ^ "}"
    | UnaryOp (op, rand) ->
        "{op,1,'"
        ^ Primitives.string_of_unary_op op
        ^ "', " ^ to_string rand ^ "}"
    | Variable name -> string_of_var name
    | Maybe exprs ->
        "{'maybe',1," ^ comma_separated_list string_of_maybe exprs ^ "}"
    | MaybeElse (exprs, else_clauses) ->
        "{'maybe',1,"
        ^ comma_separated_list string_of_maybe exprs
        ^ ", {'else',1,"
        ^ comma_separated_list string_of_clause else_clauses
        ^ "}}"
    | Comprehension _ | Bitstring _ | Block _ | Case _ | Catch _ | If _
    | Match _ | Receive _ | Record _ | Try _ ->
        Logger.simply_unreachable "TODO";
        exit 1
end

module Attribute = struct
  let to_string (attr : t) : string =
    match attr with
    | ExportAttr exports ->
        "{attribute,1,export,"
        ^ comma_separated_list
            (fun (name, arity) ->
              "{" ^ escape_atom name ^ "," ^ string_of_int arity ^ "}")
            exports
        ^ "}"
    | ImportAttr (module_name, imports) ->
        "{attribute,1,import,{" ^ escape_atom module_name ^ ","
        ^ comma_separated_list
            (fun (name, arity) ->
              "{" ^ escape_atom name ^ "," ^ string_of_int arity ^ "}")
            imports
        ^ "}}"
    | ModuleAttr name -> "{attribute,1,module," ^ escape_atom name ^ "}"
    | FileAttr (filename, line) ->
        "{attribute,1,file,{" ^ escape_string filename ^ ","
        ^ string_of_int line ^ "}}"
    | Eof line -> "{eof," ^ string_of_int line ^ "}"
    | Function { name; arity; clauses } ->
        "{function,1," ^ escape_atom name ^ "," ^ string_of_int arity ^ ","
        ^ comma_separated_list Expression.string_of_clause clauses
        ^ "}"
    | _ ->
        Logger.simply_unreachable "TODO";
        exit 1
end
