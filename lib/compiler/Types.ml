module Form = Beam.Core.Form (Beam.Core.Erlang)
module Set = BatSet

type predicate_name = Ast.head [@@deriving show, ord]
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
  type raw = string -> string -> unit
  type both = { beam : t; executable : raw }
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
  query : predicate_name Location.with_location option;
  hidden : hidden_definitions option;
}

and comptime = Module of compiled_module | Signature of compiled_signature

type scope = comptime nested_env
type sig_scope = signature nested_env

module type LOOKUP = sig
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

module Options = struct
  type sakura = { root_module : string; address : string; port : int }

  let initialize_sakura ?(root_module = "db") ~address ~port () : sakura =
    { root_module; address; port }

  type artifact =
    | Library
    | Executable of { root_module : string; filename : string }

  type t = { sakura : sakura option; artifact : artifact }

  let initialize ?(sakura = None) ~artifact () : t = { sakura; artifact }
end

type 'state t = {
  state : 'state;
  externals : comptime env;
  imports : BatSet.String.t;
  header : forms;
  output : forms;
  filename : string;
  module_name : string;
  parent : 'state t option;
  env : compiled_module;
  persist : Persist.t;
  lookup : (module LOOKUP with type t = 'state t);
}

type initialization = {
  persist : Persist.t;
  filename : string;
  externals : comptime env;
}

type 'a initialize_nested =
  initialization -> BatSet.String.t -> 'a t option -> string -> 'a t

type ('state, 'directives, 'mods) runner = {
  step : ('directives, 'mods) Ast.Clause.t list * 'state t -> 'state t;
  initialize_nested : 'state initialize_nested;
}

module type COMPILER_CONFIG = sig
  include Preprocessor.TARGET

  type state

  val initial_state : unit -> state

  val compile_clause :
    (state, directives, mods) runner ->
    (directives, mods) Ast.Clause.t ->
    state t ->
    state t

  module Lookup : LOOKUP with type t = state t
end

module type COMPILER = sig
  include Preprocessor.PREPROCESSOR

  type state

  val step : (directives, mods) Ast.Clause.t list * state t -> state t
  val initialize : initialization -> state t
end

module Make (Config : COMPILER_CONFIG) :
  COMPILER
    with type state = Config.state
    with type directives = Config.directives
    with type mods = Config.mods = struct
  include Config
  include Preprocessor.Make (Config)

  let initialize_nested ({ persist; filename; externals } : initialization)
      imports parent module_name : state t =
    {
      state =
        Option.fold ~none:(initial_state ()) ~some:(fun p -> p.state) parent;
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
          query = None;
        };
      persist;
      lookup = (module Config.Lookup);
    }

  let initialize ({ filename; _ } as init : initialization) : state t =
    let module_name = ModuleName.of_filepath filename in
    initialize_nested init BatSet.String.empty None module_name

  let rec step : (directives, mods) Ast.Clause.t list * state t -> state t =
    function
    | [], compiler ->
        if not @@ FT.is_empty compiler.output then
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
