type entry_point = { p_register : int }
type functor_name = string * int [@@deriving ord]

module Form = Beam.Core.Form (Beam.Core.Erlang)
module FT = BatFingerTree
module Set = BatSet

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

type functor_map = int FunctorMap.t

let show_functor_table (functors : functor_map) : string =
  let open FunctorMap in
  BatSeq.fold_left
    (fun acc ((label, arity), address) ->
      acc ^ label ^ "/" ^ string_of_int arity ^ ":" ^ string_of_int address
      ^ "\n")
    "" (to_seq functors)

type forms = Form.t FT.t

type t = {
  current_file : string;
  header : forms;
  output : forms;
  defined_symbols : (Ast.tag * int) BatSet.t;
}

let initialize filename : t =
  let module_name = Filename.basename @@ Filename.chop_extension filename in
  {
    current_file = filename;
    defined_symbols = BatSet.empty;
    header =
      FT.of_list
        [
          Beam.Builder.Attribute.file filename 1;
          (* TODO: this should be a proper atom *)
          Beam.Builder.Attribute.module_ module_name;
        ];
    output = FT.empty;
  }

let rec compile_expr (expr : Ast.expr) : Beam.Builder.Expr.t =
  let open Ast in
  let open Beam in
  match Location.strip_loc expr with
  | Variable var -> Builder.var var.namev
  | Functor { namef; arity; _ } when arity = 0 -> Builder.atom namef
  | Functor { namef; elements; _ } ->
      let name = Builder.atom namef in
      Builder.tuple (name :: List.map compile_expr elements)
  | Integer number -> Builder.int number

let call_with_fresh ({ namev } : Ast.var) expr =
  let open Beam in
  Ukanren.call_with_fresh @@ Builder.lambda namev expr

let compile_declaration_bodies (clauses : Ast.decl Location.with_location list)
    =
  if List.is_empty clauses then (
    Logger.simply_unreachable "Predicates must have at least one body";
    exit 1)
  else
    let open Beam in
    let compile_single_body ({ content; _ } : Ast.decl Location.with_location) :
        Builder.Expr.t =
      let open Ast in
      let find_variables func = Preprocessor.find_variables (Functor func) in
      let vars =
        content.body
        |> List.map (Fun.compose find_variables Location.strip_loc)
        |> List.fold_left Set.union Set.empty
        |> Set.filter (fun { namev } ->
               Str.string_match (Str.regexp "^[A-Z]") namev 0)
      in
      let open Location in
      let body =
        (* TODO: We should use locations when calling Beam helpers. They don't use
           locations yet, hence they are not being sent as arguments *)
        let make_function { content = { namef; elements; arity }; loc } =
          let args = List.map compile_expr elements in
          match (namef, arity) with
          | "eq", 2 -> (
              match args with
              | expr1 :: expr2 :: _ -> Ukanren.eq expr1 expr2
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'eq'";
                  exit 1)
          | "nat", 1 -> (
              match args with
              | expr1 :: _ -> Ukanren.nat expr1
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'nat'";
                  exit 1)
          | _ -> Builder.call (Builder.atom namef) args
        in
        content.body |> List.map make_function |> Ukanren.conj
      in
      Set.fold call_with_fresh vars body
    in
    clauses |> List.map compile_single_body |> Ukanren.disj

let compile_multi_declaration
    ((first_clause, remaining_clauses) :
      Ast.decl Location.with_location * Ast.decl Location.with_location list)
    (compiler : t) : t =
  let { namef; elements; arity } : Ast.func = first_clause.content.head in
  let declaration =
    let args = List.map Ast.Expr.extract_variable elements in
    Beam.Builder.single_function_declaration namef
      (List.map (fun v -> Beam.Builder.Pattern.Variable v) args)
    @@ compile_declaration_bodies (first_clause :: remaining_clauses)
  in
  let export = Beam.Builder.Attribute.export [ (namef, arity) ] in
  {
    compiler with
    output = FT.cons (FT.snoc compiler.output declaration) export;
  }

let compile_clause (clause : Ast.clause) (compiler : t) : t =
  (* TODO: handle location *)
  match clause.content with
  | MultiDeclaration (first, rest) ->
      let open Location in
      compile_multi_declaration
        ({ content = first; loc = clause.loc }, rest)
        compiler
  | Query { namef; arity; elements } ->
      let open Beam in
      let open Ast in
      let declaration =
        let query_args = List.map Ast.Expr.extract_variable elements in
        let fun_args =
          List.init (arity + 1) @@ Fun.const Builder.pattern_wildcard
        in
        Builder.single_function_declaration namef fun_args
        @@ Ukanren.run_lazy
        @@ List.fold_right call_with_fresh
             (List.map (fun namev -> { namev }) query_args)
        @@ List.fold_right Ukanren.query_variable query_args
        @@ Builder.call (Builder.atom "") (List.map Builder.var query_args)
      in
      let export = Beam.Builder.Attribute.export [ (namef, arity + 1) ] in
      {
        compiler with
        output = FT.cons (FT.snoc compiler.output declaration) export;
      }

(* TODO: figure out the logistics of handling the runtime lib *)
let rec compile : Ast.clause list * t -> Form.t FT.t = function
  | [], { output; header; _ } -> FT.append header output
  | clause :: remaining, compiler ->
      compile (remaining, compile_clause clause compiler)
