module Form = Beam.Core.Form (Beam.Core.Erlang)
module FT = BatFingerTree
module Set = BatSet

type predicate_name = Ast.Clause.head [@@deriving show, ord]
type forms = Form.t FT.t
type 'a env = 'a Location.with_location BatMap.String.t

module PredicateMap = BatMap.Make (struct
  type t = predicate_name [@@deriving show, ord]
end)
[@@warning "-32"]

module Persist = struct
  (* TODO: don't throw exceptions. Use a result for the return type. *)
  type t = string -> forms -> unit
end

type functor_map = int PredicateMap.t
type entry_point = { p_register : int }

type sig_env = signature env

and compiled_signature = {
  modules : sig_env;
  predicates : predicate_name Set.t;
}

and signature =
  | PlainSignature of compiled_signature
  | Abstract of int
  | ModuleSignature of compiled_signature

type hidden_definitions = {
  modules : comptime env;
  predicates : unit PredicateMap.t;
}

and compiled_module = {
  modules : comptime env;
  (* TODO: Later this will become something related to types *)
  predicates : unit PredicateMap.t;
  hidden : hidden_definitions option;
}

and comptime = Module of compiled_module | Signature of compiled_signature

type t = {
  header : forms;
  output : forms;
  filename : string;
  module_name : string;
  parent : t option;
  env : compiled_module;
  persist : Persist.t;
}

let initialize_nested persist parent filename module_name : t =
  {
    parent;
    filename;
    module_name;
    header =
      FT.of_list
        [
          Beam.Builder.Attribute.file filename 1;
          (* TODO: this should be a proper atom *)
          Beam.Builder.Attribute.module_ module_name;
        ];
    output = FT.empty;
    env =
      {
        modules = BatMap.String.empty;
        predicates = PredicateMap.empty;
        hidden = None;
      };
    persist;
  }

let initialize persist filename : t =
  let module_name = Filename.basename @@ Filename.chop_extension filename in
  initialize_nested persist None filename module_name

let show_functor_table (functors : functor_map) : string =
  let open PredicateMap in
  BatSeq.fold_left
    (fun acc ({ Ast.Clause.name; arity }, address) ->
      acc ^ name ^ "/" ^ string_of_int arity ^ ":" ^ string_of_int address
      ^ "\n")
    "" (to_seq functors)
