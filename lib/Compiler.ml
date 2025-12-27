type entry_point = { p_register : int }
type functor_name = string * int [@@deriving ord]

module Form = Beam.Core.Form (Beam.Core.Erlang)
module FT = BatFingerTree

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

let extra_arguments =
  3 (* These are additional arguments used by the runtime  *)

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
  output : forms;
  defined_symbols : (Ast.tag * int) BatSet.t;
}

let initialize filename : t =
  let module_name = Filename.chop_extension filename in
  {
    current_file = filename;
    defined_symbols = BatSet.empty;
    output =
      FT.of_list
        [
          Beam.Builder.Attribute.file filename 1;
          (* TODO: this should be a proper atom *)
          Beam.Builder.Attribute.module_ module_name;
        ];
  }

let rec compile_body (clauses : Ast.decl Location.with_location list)
    (_count : int) =
  match clauses with
  | [] ->
      Logger.simply_unreachable "Predicates must have at least one body";
      exit 1
  | [ { content; loc = _ } ] ->
      let { head = _; body = _ } : Ast.decl = content in
      [] (* TODO: must not create a choice point *)
  | _first_clause :: _remaining -> [] (* TODO: must create a choice point *)

let compile_declaration
    ((first_clause, remaining_clauses) :
      Ast.decl Location.with_location * Ast.decl Location.with_location list)
    (compiler : t) : t =
  let { namef; arity; _ } : Ast.func = first_clause.content.head in
  let declaration =
    Beam.Builder.function_declaration namef (arity + extra_arguments)
    @@ compile_body (first_clause :: remaining_clauses) 0
  in
  { compiler with output = FT.snoc compiler.output declaration }

let compile_form (form : Ast.clause) (compiler : t) : t =
  (* TODO: handle location *)
  match form.content with
  | MultiDeclaration (first, rest) ->
      let open Location in
      compile_declaration ({ content = first; loc = form.loc }, rest) compiler
  | Query _ -> compiler

(* TODO: figure out the logistics of handling the runtime lib *)
let rec compile : Ast.clause list * t -> Form.t FT.t = function
  | [], { output; _ } -> output
  | form :: remaining, compiler ->
      compile (remaining, compile_form form compiler)
