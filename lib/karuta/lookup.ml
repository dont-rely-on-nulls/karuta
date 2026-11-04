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

let nested_env ({ env; state = { imports }; externals; filename; _ } : t) :
    Shared.Compiler.compiled_module =
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
              Shared.Compiler.Module
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
        if BatSet.String.mem import_name Shared.Compiler.builtin_module_names
        then
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
