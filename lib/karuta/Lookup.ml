include Types
include Shared.Lookup

type dependent = { index : int }

type cycle_detection = {
  dependents : dependent BatMap.String.t;
  trace : string FT.t;
}

let files_being_compiled : cycle_detection External.Dynvar.dynvar =
  External.Dynvar.dnew
    ~init:{ dependents = BatMap.String.empty; trace = FT.empty }
    ()

let check_dependency_cycle (filepath : string) (f : unit -> 'a) : 'a =
  let { dependents; trace } = External.Dynvar.dref files_being_compiled in
  match BatMap.String.find_opt filepath dependents with
  | Some { index } ->
      Logger.with_min_level Logger.Level.Error @@ fun () ->
      let _, cycle_start = FT.split_at trace index in
      Logger.simply_error
      @@ BatIO.to_string
           (FT.print ~first:"Dependency cycle detected:\n" ~sep:"\n→  " ~last:""
              BatIO.nwrite)
           (FT.snoc cycle_start filepath);
      exit 1
  | None ->
      External.Dynvar.dlet files_being_compiled
        {
          dependents =
            BatMap.String.add filepath
              { index = BatMap.String.cardinal dependents }
              dependents;
          trace = FT.snoc trace filepath;
        }
        f

type t = state Shared.Compiler.t

let rec print_module externals =
  BatMap.String.print BatInnerIO.write_string
    (fun out { Location.content; _ } ->
      match content with
      | Shared.Compiler.Module { modules; predicates; _ } ->
          print_module modules;
          Shared.Compiler.PredicateMap.print ~first:"|" ~last:"|"
            (fun out h -> BatInnerIO.write_string out (Ast.show_head h))
            (fun out _ -> BatInnerIO.write_string out "()")
            out predicates
      | Signature _ -> BatInnerIO.write_string out "<Sig>")
    BatInnerIO.stderr externals

let nested_env ({ env; state = { imports }; externals; filename; _ } : t) :
    Shared.Compiler.compiled_module =
  let import_without_shadowing import_name (import_loc : Location.region)
      (module_env : Shared.Compiler.comptime Shared.Compiler.env) :
      Shared.Compiler.comptime Shared.Compiler.env =
    match
      ( BatMap.String.find_opt import_name externals,
        BatMap.String.find_opt import_name module_env )
    with
    | None, _ ->
        Logger.error import_loc "Attempt to import a file that does not exist";
        exit 1
    | Some { content = dependency; loc }, None ->
        let compiled () =
          {
            Location.content = Shared.Compiler.Module (Lazy.force dependency);
            loc = import_loc;
          }
        in
        BatMap.String.add import_name
          (if Lazy.is_done dependency then compiled ()
           else check_dependency_cycle loc.filename compiled)
          module_env
    | Some _, Some { Location.loc; _ } when import_loc = Location.dummy ->
        (* If the import does not have a location, that means the compiler inserted it
           for a builtin module. This is inserted in all scopes, so we must not error out.
           If the user tries to shadow it there will be an error elsewhere. *)
        module_env
    | Some _, Some { Location.loc; _ } ->
        Logger.error import_loc
          "Attempt to shadow an existing name with an import";
        if BatSet.String.mem import_name Shared.Compiler.builtin_module_names
        then
          Logger.simply_error @@ "Attempt to shadow " ^ import_name
          ^ ", which is a builtin"
        else Logger.error loc "Previous definition here";
        exit 1
  in
  {
    env with
    modules = BatMap.String.fold import_without_shadowing imports env.modules;
  }
