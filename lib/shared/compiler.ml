module Form = Beam.Core.Form (Beam.Core.Erlang)
module Set = BatSet

type predicate_name = Ast.head [@@deriving show, ord]
type forms = Form.t FT.t
type 'a env = 'a Location.with_location BatMap.String.t

let join_qualifiers names : string =
  BatIO.to_string
    (FT.print ~first:"" ~last:"" ~sep:Module_name.separator BatIO.nwrite)
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
  loc : Location.region;
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

let builtin_module ~predicates ~name ~modules =
  let qualifier = (FT.empty, name) in
  Location.add_loc
    {
      qualifier;
      query = None;
      modules;
      predicates =
        PredicateMap.of_list
        @@ List.map
             (fun name ->
               (name, { original_module = qualifier; loc = Location.dummy }))
             predicates;
    }
    Location.dummy

let builtin_module_names = BatSet.String.of_list [ "karuta"; "sakura" ]

let karuta_builtins : compiled_module Location.with_location =
  builtin_module ~name:"karuta" ~modules:BatMap.String.empty
    ~predicates:
      [
        { name = "dee"; arity = 0 };
        { name = "dum"; arity = 0 };
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

type dependent = { index : int }

type cycle_detection = {
  dependents : dependent BatMap.String.t;
  trace : string FT.t;
}

let files_being_compiled : cycle_detection Eio.Fiber.key =
  Eio.Fiber.create_key ()

let check_dependency_cycle (filepath : string) (f : unit -> 'a) : 'a =
  let { dependents; trace } =
    Option.fold
      ~none:{ dependents = BatMap.String.empty; trace = FT.empty }
      ~some:Fun.id
      (Eio.Fiber.get files_being_compiled)
  in
  match BatMap.String.find_opt filepath dependents with
  | Some { index } ->
      Logger.with_min_level Logger.Level.Error @@ fun () ->
      let _, cycle_start = FT.split_at trace index in
      Logger.simply_error
      @@ BatIO.to_string
           (FT.print ~first:"Dependency cycle detected:\n" ~sep:"\n→  " ~last:""
              BatIO.nwrite)
           (FT.snoc cycle_start filepath);
      raise Exit
  | None ->
      Eio.Fiber.with_binding files_being_compiled
        {
          dependents =
            BatMap.String.add filepath
              { index = BatMap.String.cardinal dependents }
              dependents;
          trace = FT.snoc trace filepath;
        }
        f

module Options = struct
  type sakura = { root_module : string; address : string; port : int }

  let initialize_sakura ?(root_module = "db") ~address ~port () : sakura =
    { root_module; address; port }

  type executable = { root_module : string; filename : string }
  type artifact = Library | Executable of executable
  type t = { sakura : sakura option; artifact : artifact }

  let initialize ?(sakura = None) ~artifact () : t = { sakura; artifact }
end

type externals = compiled_module option Lazy.t env

let rec print_module externals =
  BatMap.String.print BatInnerIO.write_string
    (fun out { Location.content; _ } ->
      match content with
      | Module { modules; predicates; _ } ->
          print_module modules;
          PredicateMap.print ~first:"|" ~last:"|"
            (fun out h -> BatInnerIO.write_string out (Ast.show_head h))
            (fun out _ -> BatInnerIO.write_string out "()")
            out predicates
      | Signature _ -> BatInnerIO.write_string out "<Sig>")
    BatInnerIO.stderr externals

type 'state t = {
  state : 'state;
  externals : externals;
  header : forms;
  output : forms;
  filename : string;
  parent : 'state t option;
  env : compiled_module;
  persist : Persist.t;
}

type initialization = {
  persist : Persist.t;
  filename : string;
  externals : externals;
  imports : Ast.imports;
}

type 'state initialize_nested =
  initialization -> 'state t option -> string -> 'state t

type ('state, 'directives) step =
  'directives Ast.Module.module_body * 'state t -> compiled_module

type ('state, 'directives) runner = {
  step : ('state, 'directives) step;
  initialize_nested : 'state initialize_nested;
}

module type COMPILER_CONFIG = sig
  type directives
  type state

  val init_state : unit -> state

  val compile_declaration :
    Ast.head ->
    Ast.Module.decl Location.with_location
    * Ast.Module.decl Location.with_location FT.t ->
    state t ->
    state t

  val compile_directive :
    (state, directives) runner ->
    state t ->
    directives Ast.Module.directive Location.with_location ->
    state t

  val compile_query :
    Ast.Module.query_ref Location.with_location option -> state t -> state t

  module Preprocessor :
    Preprocessor.PREPROCESSOR_CONFIG with type directives = directives
end

module type COMPILER = sig
  type directives
  type state

  val preprocess_clauses :
    Preprocessor.t -> Ast.ParserClause.t FT.t -> directives Preprocessor.output

  val compile_files : Persist.both -> BatSet.String.t -> externals -> externals
  val initialize : initialization -> state t
end

module Make (Config : COMPILER_CONFIG) :
  COMPILER
    with type state = Config.state
    with type directives = Config.directives = struct
  include (
    Preprocessor.Make
      (Config.Preprocessor) :
        Preprocessor.PREPROCESSOR with type directives = Config.directives)

  type state = Config.state
  type directives = Config.directives

  let nested_env ({ env; externals; filename; _ } : state t) imports :
      compiled_module =
    Eio.Switch.run @@ fun sw ->
    let import_without_shadowing import_name (import_loc : Location.region)
        additions =
      match
        ( BatMap.String.find_opt import_name externals,
          BatMap.String.find_opt import_name env.modules )
      with
      | None, _ ->
          Logger.error import_loc "Attempt to import a file that does not exist";
          raise Exit
      | Some { content = dependency; loc }, None ->
          let compiled () =
            Eio.Fiber.fork_promise ~sw @@ fun () ->
            {
              Location.content =
                Module
                  (match Lazy.force dependency with
                  | Some v -> v
                  | None -> raise Exit);
              loc = import_loc;
            }
          in
          FT.snoc additions
            ( import_name,
              if Lazy.is_done dependency then compiled ()
              else check_dependency_cycle loc.filename compiled )
      | Some _, Some { Location.loc; _ } when import_loc = Location.dummy ->
          (* If the import does not have a location, that means the compiler inserted it
           for a builtin module. This is inserted in all scopes, so we must not error out.
           If the user tries to shadow it there will be an error elsewhere. *)
          additions
      | Some _, Some { Location.loc; _ } ->
          Logger.error import_loc
            "Attempt to shadow an existing name with an import";
          if BatSet.String.mem import_name builtin_module_names then
            Logger.simply_error @@ "Attempt to shadow " ^ import_name
            ^ ", which is a builtin"
          else Logger.error loc "Previous definition here";
          raise Exit
    in
    let additions =
      BatMap.String.fold import_without_shadowing imports FT.empty
    in
    {
      env with
      modules =
        FT.fold_left
          (fun acc (name, import) ->
            BatMap.String.add name (Eio.Promise.await_exn import) acc)
          env.modules additions;
    }

  let initialize_nested
      ({ persist; filename; externals; imports } : initialization) parent
      module_name : Config.state t =
    let state, env =
      Option.fold
        ~none:
          ( Config.init_state (),
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
          (p.state, { p.env with qualifier }))
        parent
    in
    let full_module_name =
      join_qualifiers @@ ft_of_original_module env.qualifier
    in
    let nested_compiler =
      {
        state;
        parent;
        externals =
          BatMap.String.add "karuta"
            (Location.fmap
               (Fun.compose Lazy.from_val Option.some)
               karuta_builtins)
            externals;
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
      }
    in
    { nested_compiler with env = nested_env nested_compiler imports }

  let initialize ({ filename; _ } as init : initialization) : Config.state t =
    let module_name = Module_name.of_filepath filename in
    initialize_nested init None module_name

  let rec step : (Config.state, Config.directives) step =
   fun ({ declarations; directives; query; _ }, compiler) ->
    let forbid_shadowing _ lhs rhs =
      match (lhs, rhs) with
      | None, None -> None
      | (Some _ as lhs), None -> lhs
      | None, (Some _ as rhs) -> rhs
      | Some { loc = lhs_loc; _ }, Some { loc = rhs_loc; _ } ->
          Logger.error rhs_loc "Attempt to shadow a predicate";
          Logger.error lhs_loc "Outer definition here";
          raise Exit
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
    compiler.env

  let parse = function
    | "" ->
        Logger.simply_error "Input filepath is empty";
        raise Exit
    | str ->
        In_channel.with_open_text str @@ fun inc ->
        if in_channel_length inc = 0 then (
          Logger.simply_error @@ "Input file '" ^ str ^ "' is empty";
          raise Exit)
        else Parser.parse str (In_channel.input_all inc)

  let compile_files : Persist.both -> BatSet.String.t -> externals -> externals
      =
   fun persist filenames precompiled ->
    let (env, bind_env) : externals Eio.Promise.t * externals Eio.Promise.u =
      Eio.Promise.create ()
    in
    let compile_one filepath acc =
      let module_name = Module_name.of_filepath filepath in
      let step () =
        Logger.in_file filepath @@ fun () ->
        try
          let preprocessor = Preprocessor.initialize filepath in
          let { Preprocessor.module_ = body } =
            filepath |> parse |> preprocess_clauses preprocessor
          in
          Option.some
          @@ step
               ( body,
                 initialize
                   {
                     persist = persist.beam;
                     filename = filepath;
                     externals = Eio.Promise.await env;
                     imports = body.imports;
                   } )
        with Exit -> None
      in
      let new_module = Lazy.from_fun step in
      BatMap.String.add module_name
        (Location.add_loc new_module
        @@ Location.double filepath
             (* TODO: make the endl actually point to the end of the file *)
             Location.origin)
        acc
    in
    precompiled
    |> BatSet.String.fold compile_one filenames
    |> Eio.Promise.resolve bind_env;
    Eio.Promise.await env
end
