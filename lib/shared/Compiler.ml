module Form = Beam.Core.Form (Beam.Core.Erlang)
module Set = BatSet

type predicate_name = Ast.head [@@deriving show, ord]
type forms = Form.t FT.t
type 'a env = 'a Location.with_location BatMap.String.t
type 'a nested_env = 'a env BatLazyList.t

let join_qualifiers names : string =
  BatIO.to_string
    (FT.print ~first:"" ~last:"" ~sep:ModuleName.separator BatIO.nwrite)
    names

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

type predicate = {
  (* TODO: add type information *)
  original_module : string FT.t * string;
  loc : Location.location;
}

let ft_of_original_module : string FT.t * string -> string FT.t =
  BatPervasives.uncurry FT.snoc

type sig_env = signature env

and compiled_signature = {
  modules : sig_env;
  predicates : predicate_name Set.t;
}

and signature =
  | PlainSignature of compiled_signature
  | Abstract of int
  | ModuleSignature of compiled_signature

and compiled_module = {
  qualifier : string FT.t * string;
  modules : comptime env;
  predicates : predicate PredicateMap.t;
  query : predicate_name Location.with_location option;
}

and comptime = Module of compiled_module | Signature of compiled_signature

let rec signature_equal lhs rhs =
  match (lhs, rhs) with
  | PlainSignature lhs, PlainSignature rhs
  | ModuleSignature lhs, ModuleSignature rhs ->
      Set.equal lhs.predicates rhs.predicates
      && BatMap.String.equal
           (fun { Location.content = lhs; _ } { content = rhs; _ } ->
             signature_equal lhs rhs)
           lhs.modules rhs.modules
  | Abstract lhs, Abstract rhs -> lhs = rhs
  | _ -> false

let builtin_module name predicates =
  let qualifier = (FT.empty, name) in
  Location.add_loc
    (Module
       {
         qualifier;
         query = None;
         modules = BatMap.String.empty;
         predicates =
           PredicateMap.of_list
           @@ List.map
                (fun name ->
                  (name, { original_module = qualifier; loc = Location.dummy }))
                predicates;
       })
    Location.dummy

let karuta_builtins : comptime Location.with_location =
  builtin_module "karuta"
    [
      { name = "t-dee"; arity = 0 };
      { name = "t-dum"; arity = 0 };
      { name = "int"; arity = 1 };
      { name = "nat"; arity = 1 };
      { name = "eq"; arity = 2 };
      { name = "leq"; arity = 2 };
      { name = "lt"; arity = 2 };
      { name = "neg"; arity = 2 };
      { name = "minus"; arity = 3 };
      { name = "mult"; arity = 3 };
      { name = "plus"; arity = 3 };
      { name = "divmod"; arity = 4 };
    ]

module type LOOKUP = sig
  type t

  val comptime_of_compiler : t -> comptime Location.with_location

  val signature :
    comptime Location.with_location ->
    Ast.Expr.func_label ->
    [> `Ok of compiled_signature Location.with_location
    | `Undefined of string Location.with_location
    | `UnexpectedModule of compiled_module Location.with_location
    | `UnexpectedSignature of Location.location ]

  val m0dule :
    comptime Location.with_location ->
    Ast.Expr.func_label ->
    [> `Ok of compiled_module Location.with_location
    | `Undefined of string Location.with_location
    | `UnexpectedSignature of Location.location ]

  val nested_signature :
    sig_env Location.with_location ->
    comptime Location.with_location ->
    Ast.Expr.func_label ->
    [> `Ok of signature Location.with_location
    | `Undefined of string Location.with_location
    | `UnexpectedModule of compiled_module Location.with_location
    | `UnexpectedSignature of Location.location ]

  val predicate :
    comptime Location.with_location ->
    compiled_module ->
    Ast.Expr.func_label ->
    int ->
    [> `Ok of predicate
    | `Undefined of string Location.with_location
    | `UnexpectedSignature of Location.location ]
end

module Options = struct
  type sakura = { root_module : string; address : string; port : int }

  let initialize_sakura ?(root_module = "db") ~address ~port () : sakura =
    { root_module; address; port }

  type executable = { root_module : string; filename : string }
  type artifact = Library | Executable of executable
  type t = { sakura : sakura option; artifact : artifact }

  let initialize ?(sakura = None) ~artifact () : t = { sakura; artifact }
end

type 'state t = {
  state : 'state;
  externals : comptime env;
  header : forms;
  output : forms;
  filename : string;
  parent : 'state t option;
  env : compiled_module;
  persist : Persist.t;
  lookup : (module LOOKUP with type t = 'state t);
}

type 'mods initialization = {
  persist : Persist.t;
  filename : string;
  externals : comptime env;
  mods : 'mods;
}

type ('state, 'mods) initialize_nested =
  'mods initialization -> 'state t option -> string -> 'state t

type ('state, 'directives, 'mods) step =
  ('directives, 'mods) Ast.Module.module_body * 'state t -> 'state t

type ('state, 'directives, 'mods) runner = {
  step : ('state, 'directives, 'mods) step;
  initialize_nested : ('state, 'mods) initialize_nested;
}

module type COMPILER_CONFIG = sig
  type directives
  type mods
  type state

  val init_state : mods -> state
  val merge_state : mods -> state -> state

  val compile_declaration :
    Ast.head ->
    Ast.Module.decl Location.with_location
    * Ast.Module.decl Location.with_location FT.t ->
    state t ->
    state t

  val compile_directive :
    (state, directives, mods) runner ->
    state t ->
    (directives, mods) Ast.Module.directive Location.with_location ->
    state t

  val compile_query :
    Ast.Module.query_ref Location.with_location option -> state t -> state t

  module Lookup : LOOKUP with type t = state t

  module Preprocessor :
    Preprocessor.PREPROCESSOR_CONFIG
      with type directives = directives
      with type mods = mods
end

module type COMPILER = sig
  type directives
  type mods
  type state

  val preprocess_clauses :
    Preprocessor.t ->
    Ast.ParserClause.t FT.t ->
    (directives, mods) Preprocessor.output

  val compile_files :
    Persist.both ->
    (directives, mods) Ast.Module.module_body BatMap.String.t ->
    comptime env ->
    string FT.t ->
    comptime env

  val initialize : mods initialization -> state t
end

module Make (Config : COMPILER_CONFIG) :
  COMPILER
    with type state = Config.state
    with type directives = Config.directives
    with type mods = Config.mods = struct
  type state = Config.state
  type directives = Config.directives
  type mods = Config.mods

  let initialize_nested
      ({ persist; filename; externals; mods } : mods initialization) parent
      module_name : Config.state t =
    let state, env =
      Option.fold
        ~none:
          ( Config.init_state mods,
            {
              qualifier = (FT.empty, module_name);
              modules = BatMap.String.empty;
              predicates = PredicateMap.empty;
              query = None;
            } )
        ~some:(fun p ->
          let qualifier =
            (ft_of_original_module p.env.qualifier, module_name)
          in
          (Config.merge_state mods p.state, { p.env with qualifier }))
        parent
    in
    let full_module_name =
      join_qualifiers @@ ft_of_original_module env.qualifier
    in
    {
      state;
      parent;
      externals = BatMap.String.add "karuta" karuta_builtins externals;
      filename;
      header =
        FT.of_list
          [
            Beam.Builder.Attribute.file filename 1;
            (* TODO: this should be a proper atom *)
            Beam.Builder.Attribute.module_ full_module_name;
          ];
      output = FT.empty;
      env;
      persist;
      lookup = (module Config.Lookup);
    }

  let initialize ({ filename; _ } as init : mods initialization) :
      Config.state t =
    let module_name = ModuleName.of_filepath filename in
    initialize_nested init None module_name

  let rec step : (Config.state, Config.directives, Config.mods) step =
   fun ({ declarations; directives; query; _ }, compiler) ->
    let forbid_shadowing _ lhs rhs =
      match (lhs, rhs) with
      | None, None -> None
      | (Some _ as lhs), None -> lhs
      | None, (Some _ as rhs) -> rhs
      | Some { loc = lhs_loc; _ }, Some { loc = rhs_loc; _ } ->
          Logger.error rhs_loc "Attempt to shadow a predicate";
          Logger.error lhs_loc "Outer definition here";
          exit 1
    in
    let local_predicates : predicate PredicateMap.t =
      declarations |> BatMap.enum
      |> BatEnum.map
           (fun (k, (({ loc; _ }, _) : 'b Location.with_location * 'a)) ->
             (k, { original_module = compiler.env.qualifier; loc }))
      |> PredicateMap.of_enum
    in
    let compiler =
      FT.fold_left
        (Config.compile_directive { step; initialize_nested })
        {
          compiler with
          env =
            {
              compiler.env with
              predicates =
                PredicateMap.merge forbid_shadowing compiler.env.predicates
                  local_predicates;
            };
        }
        directives
      |> BatMap.foldi Config.compile_declaration declarations
      |> Config.compile_query query
    in
    if not @@ FT.is_empty compiler.output then
      compiler.persist compiler.filename
        (FT.append compiler.header compiler.output);
    if Option.is_none compiler.parent then
      {
        compiler with
        externals =
          BatMap.String.add
            (snd compiler.env.qualifier)
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

  let preprocess_clauses =
    let module TargetPreprocessor :
      Preprocessor.PREPROCESSOR
        with type directives = Config.directives
        with type mods = Config.mods =
      Preprocessor.Make (Config.Preprocessor)
    in
    TargetPreprocessor.preprocess_clauses

  let compile_one_file (persist : Persist.both) preprocessed externals filepath
      =
    match BatMap.String.find_opt filepath preprocessed with
    | None ->
        Logger.simply_unreachable "We hit a file that doesn't exist";
        exit 1
    | Some body ->
        (step
           ( body,
             initialize
               {
                 persist = persist.beam;
                 filename = filepath;
                 externals;
                 mods = body.target_specific;
               } ))
          .externals

  let compile_files persist preprocessed_files =
    FT.fold_left @@ compile_one_file persist preprocessed_files
end
