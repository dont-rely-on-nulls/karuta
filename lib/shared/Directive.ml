open Compiler

let create_nested_module_name module_name parent : string =
  parent.module_name ^ ModuleName.separator ^ module_name

let initialize_from_parent (type a) module_name
    (initialize_nested : a initialize_nested) parent : a t =
  let inner_module_name = create_nested_module_name module_name parent in
  let inner_filename =
    ModuleName.of_filepath parent.filename ^ "." ^ module_name ^ ".krt"
  in
  initialize_nested
    {
      externals = parent.externals;
      persist = parent.persist;
      filename = inner_filename;
    }
    (Some parent) inner_module_name

let create_clause
    (directives :
      ('directives, 'mods) Ast.Module.directive Location.with_location FT.t)
    (declarations : Ast.Module.multi_declaration) :
    ('directives, 'mods) Ast.Module.t FT.t =
  let lifted_declarations =
    FT.map (Location.fmap (fun c -> Ast.Module.MultiDeclaration c)) declarations
  in
  let lifted_directives =
    FT.map (Location.fmap (fun d -> Ast.Module.Directive d)) directives
  in
  FT.append lifted_directives lifted_declarations

let rec compile : type a.
    Location.location ->
    ('directives, 'mods) Ast.Module.directive ->
    (('directives, 'mods) Ast.Module.t FT.t * a t -> a t) ->
    a t ->
    a Compiler.initialize_nested ->
    a t =
 fun directive_loc directive step
     ({ env = { modules; _ } as env; _ } as compiler) initialize_nested ->
  let module Lookup = (val compiler.lookup) in
  match directive with
  | Module
      {
        name = { content = module_name; loc = module_loc } as name;
        signature = None;
        declarations;
        directives;
        _;
      } -> (
      let module_name' = (FT.empty, name) in
      let comptime_value = Lookup.ancestors_of_compiler compiler in
      match Lookup.m0dule comptime_value module_name' with
      | `Ok { loc; _ } | `UnexpectedSignature loc ->
          Logger.error module_loc "Failed to define module";
          Logger.error loc
            "There's already a module or signatures with the same name. \
             Modules and signatures share their scope.";
          exit 1
      | `Undefined _ ->
          let body = create_clause directives declarations in
          let compiled_module =
            compiler |> initialize_from_parent module_name initialize_nested
            |> fun c ->
            step (body, c) |> fun c ->
            Location.add_loc (Module c.env) directive_loc
          in
          {
            compiler with
            env =
              {
                env with
                modules = BatMap.String.add module_name compiled_module modules;
              };
          })
  | Module
      ({
         name = { content = module_name; _ } as name;
         signature = Some (Inlined signature_body);
         _;
       } as module_body) -> (
      (* TODO: add locations to each body *)
      let inline_sig =
        Signature.compile directive_loc signature_body compiler
      in
      let module_without_named_signature =
        Ast.Module.Module { module_body with signature = None }
      in
      let ({ env = { modules; _ } as env; _ } as compiler) =
        compile directive_loc module_without_named_signature step compiler
          initialize_nested
      in
      let module_name' = (FT.empty, name) in
      let comptime_value = Lookup.ancestors_of_compiler compiler in
      match Lookup.m0dule comptime_value module_name' with
      | `Ok content ->
          {
            compiler with
            env =
              {
                env with
                modules =
                  BatMap.String.add module_name
                    (inline_sig
                    |> Location.fmap (fun s -> PlainSignature s)
                    |> Signature.ascribe_to_module content
                    |> Location.fmap (fun m -> Module m))
                    modules;
              };
          }
      | _ ->
          Logger.simply_unreachable
            "Take care of non-oks when doing lookups in modules";
          exit 1)
  | Module
      {
        name = { content = module_name; loc = module_loc } as name;
        signature = Some (Named signature_name);
        declarations;
        directives;
        _;
      } -> (
      let module_name' = (FT.empty, name) in
      let comptime_value = Lookup.ancestors_of_compiler compiler in
      match
        ( Lookup.m0dule comptime_value module_name',
          Lookup.signature comptime_value signature_name.content )
      with
      | `Ok { loc; _ }, _ | `UnexpectedSignature loc, _ ->
          Logger.error module_loc "Failed to define module";
          Logger.error loc
            "There's already a module or signatures with the same name. \
             Modules and signatures share their scope.";
          exit 1
      | `Undefined _, `Ok signature ->
          let body = create_clause directives declarations in
          let comptime =
            compiler |> initialize_from_parent module_name initialize_nested
            |> fun c ->
            step (body, c) |> fun c -> Location.add_loc c.env directive_loc
          in
          let compiled_module =
            signature
            |> Location.fmap (fun v -> PlainSignature v)
            |> Signature.ascribe_to_module comptime
            |> Location.fmap (fun v -> Module v)
          in
          {
            compiler with
            env =
              {
                env with
                modules = BatMap.String.add module_name compiled_module modules;
              };
          }
      | `Undefined _, `UnexpectedModule { loc; _ } ->
          Logger.error loc "Modules are not signatures";
          exit 1
      | `Undefined _, `Undefined { loc; _ } ->
          Logger.error loc
            "This name is undefined when looking up for signatures";
          exit 1
      | `Undefined _, `UnexpectedSignature loc ->
          Logger.error loc
            "You cannot inspect other signatures when qualifying signatures. \
             Only modules can be inspected.";
          exit 1)
  | Signature
      { name = { content = name; loc }; body = { declarations; directives } }
    -> (
      match BatMap.String.find_opt name modules with
      | Some existing ->
          Logger.error loc "Failed to define signature";
          Logger.error existing.loc
            "There's already a module or signature with the same name";
          exit 1
      | None ->
          let compiled_sig =
            compiler
            |> Signature.compile directive_loc { declarations; directives }
            |> Location.fmap (fun cs -> Signature cs)
          in
          {
            compiler with
            env =
              { env with modules = BatMap.String.add name compiled_sig modules };
          })
  | TargetSpecific _ ->
      let nested_directive =
        Location.add_loc (Ast.Module.Directive directive) directive_loc
      in
      step (FT.singleton nested_directive, compiler)
