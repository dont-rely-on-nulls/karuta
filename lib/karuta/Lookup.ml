include Types
include Shared.Lookup

let files_being_compiled = External.Dynvar.dnew ~init:BatSet.String.empty ()

let check_dependency_cycle (filepath : string) (f : unit -> 'a) : 'a =
  Logger.debug @@ "Cycle check: " ^ filepath;
  let dependents = External.Dynvar.dref files_being_compiled in
  if BatSet.String.mem filepath dependents then (
    Logger.with_min_level Logger.Level.Error @@ fun () ->
    Logger.simply_error @@ "Dependency cycle detected while compiling "
    ^ filepath;
    exit 1)
  else
    External.Dynvar.dlet files_being_compiled
      (BatSet.String.add filepath dependents)
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

let comptime_of_compiler
    ({ env; state = { imports }; externals; filename; _ } : t) :
    Shared.Compiler.comptime Location.with_location =
  let forbid_shadowing key (parent_value : _ Location.with_location option)
      (external_value : _ Lazy.t Location.with_location option) :
      Shared.Compiler.comptime Location.with_location option =
    match (BatMap.String.find_opt key imports, parent_value) with
    | None, parent_value -> parent_value
    | Some import_loc, None ->
        let compiled content =
          {
            Location.content = Shared.Compiler.Module (Lazy.force content);
            loc = import_loc;
          }
        in
        Option.map
          (fun { Location.content; loc } ->
            if Lazy.is_done content then compiled content
            else
              check_dependency_cycle loc.startl.pos_fname @@ fun () ->
              compiled content)
          external_value
    | Some import_loc, Some { Location.loc; _ } ->
        Logger.error import_loc "Attempt to shadow an external import";
        Logger.error loc "Local definition here";
        exit 1
  in
  let local_env =
    {
      env with
      modules = BatMap.String.merge forbid_shadowing env.modules externals;
    }
  in
  Location.add_loc (Shared.Compiler.Module local_env) Location.dummy
