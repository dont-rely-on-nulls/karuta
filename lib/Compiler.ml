type entry_point = { p_register : int }
type functor_name = string * int [@@deriving ord]

module FT = BatFingerTree

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

type forms = Beam.Core.Form.t FT.t

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

and compile : Ast.clause list * t -> Beam.Core.Form.t FT.t = function
  | clauses, { output; _ } ->
      let convert_ast_clause (clause : Ast.clause) : Beam.Core.Form.t list =
        match clause.content with
        | Ast.MultiDeclaration ({ head; _ }, _) ->
            let func_name = head.namef in
            let arity = head.arity in
            let beam_clause = {
              Beam.Core.Form.Clause.patterns = [];
              guards = [];
              body = []
            } in
            [Beam.Core.Form.Function { name = func_name; arity; clauses = [beam_clause] }]
        | Ast.Query _ -> []
      in
      let new_forms = List.concat_map convert_ast_clause clauses in
      let export_forms = 
        let extract_exports (clause : Ast.clause) = match clause.content with
          | Ast.MultiDeclaration ({ head; _ }, _) -> Some (head.namef, head.arity)
          | _ -> None
        in
        let exports = List.filter_map extract_exports clauses in
        if exports <> [] then [Beam.Core.Form.ExportAttr exports] else []
      in
      FT.append output (FT.of_list (export_forms @ new_forms))

let serialize_to_erlang_abstract (forms : forms) : string =
  Beam.Erlang_abstract_serializer.serialize_forms forms

