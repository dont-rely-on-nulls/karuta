open Abstract_format

type abstract_form =
  | Attribute of int * string * abstract_term
  | Function of int * string * int * abstract_term list
  | Eof of int

and abstract_term =
  | Atom of string
  | Integer of int
  | String of string
  | Tuple of abstract_term list
  | List of abstract_term list
  | Cons of abstract_term * abstract_term
  | Nil

let serialize_literal = function
  | Primitives.Atom a -> Atom a
  | Primitives.Integer i -> Integer i
  | Primitives.String s -> String s
  | Primitives.Character c -> String (String.make 1 c)
  | Primitives.Float f -> String (string_of_float f)
  | Primitives.Nil -> Nil

let serialize_simple_clause patterns _body : abstract_term =
  Tuple [
    Atom "clause";
    Integer 1;
    List (List.map (fun _ -> Atom "_") patterns);
    List [];
    List [Integer 1]
  ]

let serialize_form (form : Core.Form.t) : abstract_form =
  match form with
  | FileAttr (filename, line) ->
      Attribute (line, "file", Tuple [String filename; Integer line])
  | ModuleAttr module_name ->
      Attribute (1, "module", Atom module_name)
  | ExportAttr exports ->
      let export_list = List.map (fun (name, arity) -> 
        Tuple [Atom name; Integer arity]) exports in
      Attribute (1, "export", List export_list)
  | Function { name; arity; clauses } ->
      let serialized_clauses = List.map (fun (clause : Core.Form.Clause.clause) -> 
        serialize_simple_clause clause.patterns clause.body) clauses in
      Function (1, name, arity, serialized_clauses)
  | _ ->
      Attribute (1, "unknown", Atom "unsupported_form")

let rec abstract_term_to_string = function
  | Atom s -> s
  | Integer i -> string_of_int i
  | String s -> "\"" ^ s ^ "\""
  | Tuple terms ->
      "{" ^ String.concat "," (List.map abstract_term_to_string terms) ^ "}"
  | List terms ->
      "[" ^ String.concat "," (List.map abstract_term_to_string terms) ^ "]"
  | Cons (h, t) ->
      "[" ^ abstract_term_to_string h ^ "|" ^ abstract_term_to_string t ^ "]"
  | Nil -> "[]"

let abstract_form_to_string = function
  | Attribute (line, attr_type, term) ->
      "{attribute," ^ string_of_int line ^ "," ^ attr_type ^ "," 
      ^ abstract_term_to_string term ^ "}"
  | Function (line, name, arity, clauses) ->
      "{function," ^ string_of_int line ^ "," ^ name ^ "," ^ string_of_int arity
      ^ ",[" ^ String.concat "," (List.map abstract_term_to_string clauses) ^ "]}"
  | Eof line ->
      "{eof," ^ string_of_int line ^ "}"

let serialize_forms (forms : Core.Form.t BatFingerTree.t) : string =
  let form_list = BatFingerTree.to_list forms in
  let serialized_forms = List.map serialize_form form_list in
  let eof_form = Eof 5 in
  let all_forms = serialized_forms @ [eof_form] in
  "[" ^ String.concat ",\n              " (List.map abstract_form_to_string all_forms) ^ "]"