module RegisterMap = BatMap.Make (Ast.Expr)
module VariableMap = BatMap.Make (String)
module FT = BatFingerTree

type term_queue = Ast.Expr.t FT.t

module S = BatSet

type variable_set = Ast.tag S.t
type register = Temporary of int | Permanent of int [@@deriving show]

type t = {
  x_register : int;
  y_register : int;
  registers : register RegisterMap.t;
  permanent_variables : variable_set;
}

type var_frequency_map = int VariableMap.t

let show_registers (registers : register RegisterMap.t) : string =
  let open RegisterMap in
  BatSeq.fold_left
    (fun acc (term, register) ->
      acc ^ "\n" ^ Ast.show_expr term ^ " = " ^ show_register register)
    "" (to_seq registers)
[@@warning "-32"]

let multiset_mappend (m1 : var_frequency_map) (m2 : var_frequency_map) :
    var_frequency_map =
  let mappend (_ : string) (v1 : int option) (v2 : int option) : int option =
    match (v1, v2) with
    | Some n1, Some n2 -> Some (n1 + n2)
    | None, Some n2 -> Some n2
    | Some n1, None -> Some n1
    | None, None -> None
  in
  VariableMap.merge mappend m1 m2

let set_to_multiset (s : string S.t) : var_frequency_map =
  let open VariableMap in
  let lambda elem acc = add elem 1 acc in
  S.fold lambda s VariableMap.empty

let initial_allocator : t =
  {
    x_register = 0;
    y_register = 0;
    registers = RegisterMap.empty;
    permanent_variables = S.empty;
  }

let rec allocate
    ({ registers; permanent_variables; x_register; y_register; _ } as allocator)
    (elem : Ast.expr) : t =
  let register, x_register, y_register =
    match elem with
    | Variable { namev } -> (
        match S.find_opt namev permanent_variables with
        | None -> (Temporary x_register, x_register + 1, y_register)
        | Some _ -> (Permanent y_register, x_register, y_register + 1))
    | _ -> (Temporary x_register, x_register + 1, y_register)
  in
  let open RegisterMap in
  let registers = add elem register registers in
  { allocator with registers; x_register; y_register }

and allocate_loop : t -> term_queue -> t =
 fun allocator terms ->
  match FT.front terms with
  | None -> allocator
  | Some (rest, d) -> (
      match d with
      | Variable v -> allocate_variable v allocator rest
      | Functor f -> allocate_functor f allocator rest
      | Integer _ -> allocate_loop allocator rest)

and allocate_functor : Ast.func -> t -> term_queue -> t =
 fun ({ elements; _ } as func) allocator terms ->
  let allocator = allocate allocator (Ast.Functor func) in
  let terms = FT.append terms (FT.of_list elements) in
  allocate_loop allocator terms

and allocate_variable : Ast.var -> t -> term_queue -> t =
  let open RegisterMap in
  fun var allocator terms ->
    match find_opt (Ast.Variable var) allocator.registers with
    | None -> allocate_loop (allocate allocator (Ast.Variable var)) terms
    | Some _ -> allocate_loop allocator terms

and variables_of_term (elem : Ast.expr) : string S.t =
  match elem with
  | Integer _ -> S.empty
  | Functor { elements; _ } ->
      List.fold_left
        (fun acc e -> S.union acc @@ variables_of_term e)
        S.empty elements
  | Variable { namev } -> S.add namev S.empty

and not_debug ({ namef; arity; _ } : Ast.func) : bool =
  not (namef = "debug" && arity = 0)

and allocate_declaration : Ast.decl -> t =
 fun { head = { elements; arity; _ } as head; body } ->
  let first_non_argument_register =
    List.fold_left (fun acc (f : Ast.func) -> max acc f.arity) arity body
  in
  let first_clause, other_clauses =
    match List.filter not_debug body with
    | [] -> ([], [])
    | first_clause :: other_clauses ->
        ([ Ast.Functor first_clause ], other_clauses)
  in
  let permanent_variables =
    let extracted_head_variables =
      Ast.Functor head :: first_clause
      |> List.map variables_of_term
      |> List.fold_left S.union S.empty
      |> set_to_multiset
    in
    let folder acc e =
      Ast.Functor e |> variables_of_term |> set_to_multiset
      |> multiset_mappend acc
    in
    List.fold_left folder extracted_head_variables other_clauses
    |> VariableMap.filterv (fun v -> v != 1)
    |> VariableMap.keys |> S.of_enum
  in
  let ( ++ ) = FT.append in
  let terms =
    let open Ast in
    FT.of_list elements
    ++ (FT.of_list @@ List.concat_map (fun { elements; _ } -> elements) body)
  in
  allocate_loop
    {
      initial_allocator with
      permanent_variables;
      x_register = first_non_argument_register;
    }
    terms

and allocate_query : Ast.func -> t =
 fun { elements; _ } ->
  allocate_loop
    { initial_allocator with x_register = List.length elements }
    (FT.of_list elements)

let allocate_toplevel : Ast.clause -> t list = function
  | MultiDeclaration (first, decls) ->
      allocate_declaration first :: List.map allocate_declaration decls
  | Query func -> [ allocate_query func ]
