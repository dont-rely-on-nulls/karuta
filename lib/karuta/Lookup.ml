include Types
include Shared.Lookup

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

let comptime_of_compiler ({ env; state = { imports }; externals; _ } : t) :
    Shared.Compiler.comptime Location.with_location =
  let forbid_shadowing key parent_value external_value :
      Shared.Compiler.comptime Location.with_location option =
    match (BatMap.String.find_opt key imports, parent_value) with
    | None, parent_value -> parent_value
    | Some import_loc, None ->
        Option.map
          (fun { Location.content; _ } ->
            { Location.content; loc = import_loc })
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
