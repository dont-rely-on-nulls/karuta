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
  | _, { output; _ } -> output (* TODO *)
