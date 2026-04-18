module Form = Beam.Core.Form (Beam.Core.Erlang)
module FT = BatFingerTree
module Set = BatSet

type predicate_name = Ast.Clause.head [@@deriving show, ord]
type forms = Form.t FT.t
type 'a env = 'a Location.with_location BatMap.String.t
type 'a nested_env = 'a env BatLazyList.t

module PredicateMap = BatMap.Make (struct
  type t = predicate_name [@@deriving show, ord]
end)
[@@warning "-32"]

module Persist = struct
  (* TODO: don't throw exceptions. Use a result for the return type. *)
  type t = string -> forms -> unit
end

type functor_map = int PredicateMap.t

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

type scope = comptime nested_env
type sig_scope = signature nested_env

module type LookupS = sig
  type t

  val empty_signature : sig_scope
  val sig_env_to_sig_scope : sig_env -> sig_scope
  val sig_cons : sig_scope -> sig_env -> sig_scope
  val ancestors_of_compiler : t -> scope

  val signature :
    scope ->
    Ast.Expr.func_label ->
    [> `Ok of compiled_signature Location.with_location
    | `Undefined of string Location.with_location
    | `UnexpectedModule of compiled_module Location.with_location
    | `UnexpectedSignature of Location.location ]

  val m0dule :
    scope ->
    Ast.Expr.func_label ->
    [> `Ok of compiled_module Location.with_location
    | `Undefined of string Location.with_location
    | `UnexpectedSignature of Location.location ]

  val nested_signature :
    sig_scope ->
    scope ->
    Ast.Expr.func_label ->
    [> `Ok of signature Location.with_location
    | `Undefined of string Location.with_location
    | `UnexpectedModule of compiled_module Location.with_location
    | `UnexpectedSignature of Location.location ]

  val predicate :
    scope ->
    Ast.Expr.func_label ->
    int ->
    [> `Ok of unit
    | `Undefined of string Location.with_location
    | `UnexpectedSignature of Location.location ]
end

type t = {
  externals : comptime env;
  imports : BatSet.String.t;
  header : forms;
  output : forms;
  filename : string;
  module_name : string;
  parent : t option;
  env : compiled_module;
  persist : Persist.t;
  lookup : (module LookupS with type t = t);
}

type initialization = {
  persist : Persist.t;
  filename : string;
  externals : comptime env;
}

type initialize_nested =
  initialization -> BatSet.String.t -> t option -> string -> t

type runner = {
  step : Ast.Clause.t list * t -> t;
  initialize_nested : initialize_nested;
}

module type COMPILER_CONFIG = sig
  val compile_clause : runner -> Ast.Clause.t -> t -> t

  module Lookup : LookupS with type t = t
end

module type COMPILER = sig
  val step : Ast.Clause.t list * t -> t
  val initialize : initialization -> t
end

module Make (Config : COMPILER_CONFIG) : COMPILER = struct
  let initialize_nested { persist; filename; externals } imports parent
      module_name : t =
    {
      parent;
      imports;
      externals;
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
      lookup = (module Config.Lookup);
    }

  let initialize ({ filename; _ } as init) : t =
    let module_name = ModuleName.of_filepath filename in
    initialize_nested init BatSet.String.empty None module_name

  let rec step : Ast.Clause.t list * t -> t = function
    | [], compiler ->
        compiler.persist compiler.filename
          (FT.append compiler.header compiler.output);
        if Option.is_none compiler.parent then
          {
            compiler with
            externals =
              BatMap.String.add compiler.module_name
                (let open Location in
                 add_loc (Module compiler.env)
                 @@ double
                      (* TODO: make the endl actually point to the end of the file *)
                      {
                        pos_fname = compiler.filename;
                        pos_lnum = 1;
                        pos_bol = 0;
                        pos_cnum = 1;
                      })
                compiler.externals;
          }
        else compiler
    | clause :: remaining, compiler ->
        step
          ( remaining,
            Config.compile_clause { initialize_nested; step } clause compiler )
end

let show_functor_table (functors : functor_map) : string =
  let open PredicateMap in
  BatSeq.fold_left
    (fun acc ({ Ast.Clause.name; arity }, address) ->
      acc ^ name ^ "/" ^ string_of_int arity ^ ":" ^ string_of_int address
      ^ "\n")
    "" (to_seq functors)
