open Compiler.Types

let initialize_from_parent module_name (initialize_nested : initialize_nested)
    parent : t =
  let inner_module_name =
    parent.module_name ^ Compiler.Common.module_name_separator ^ module_name
  in
  let inner_filename =
    (Filename.basename @@ Filename.chop_extension parent.filename)
    ^ "." ^ module_name ^ ".krt"
  in
  initialize_nested
    {
      externals = parent.externals;
      persist = parent.persist;
      filename = inner_filename;
    }
    parent.imports (Some parent) inner_module_name

let rec compile (directive_loc : Location.location)
    ({ elements; arity; _ } as f : Ast.Expr.func)
    (body : Ast.Clause.t list list) (step : Ast.Clause.t list * t -> t)
    ({ env = { modules; _ } as env; _ } as compiler : t)
    (initialize_nested : Compiler.Types.initialize_nested) : t =
  let module Lookup = (val compiler.lookup) in
  match (Ast.Expr.extract_func_label f, arity, body) with
  | "module", 1, [ body ] -> (
      match elements with
      | [
       {
         content =
           Ast.Expr.Functor
             {
               name = ([], { content = module_name; _ }) as module_name';
               elements = [];
               arity = 0;
             };
         loc = module_loc;
       };
      ] -> (
          let comptime_value = Lookup.ancestors_of_compiler compiler in
          match Lookup.m0dule comptime_value module_name' with
          | `Ok { loc; _ } | `UnexpectedSignature loc ->
              Logger.error module_loc "Failed to define module";
              Logger.error loc
                "There's already a module or signatures with the same name. \
                 Modules and signatures share their scope.";
              exit 1
          | `Undefined _ ->
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
                    modules =
                      BatMap.String.add module_name compiled_module modules;
                  };
              })
      | _ ->
          Logger.unreachable directive_loc
            "Somehow there is a mismatch between the expected arity and the \
             actual arity.";
          exit 1)
  | "module", 1, [ signature_body; body ] -> (
      (* TODO: add locations to each body *)
      let inline_sig =
        Signature.compile directive_loc signature_body compiler
      in
      let ({ env = { modules; _ } as env; _ } as compiler) =
        compile directive_loc f [ body ] step compiler initialize_nested
      in
      match elements with
      | [
       {
         content =
           Ast.Expr.Functor
             {
               name = ([], { content = module_name; _ }) as module_name';
               elements = [];
               arity = 0;
             };
         _;
       };
      ] -> (
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
      | _ ->
          Logger.unreachable directive_loc
            "Somehow there is a mismatch between the expected arity and the \
             actual arity.";
          exit 1)
  | "module", 2, [ body ] -> (
      match elements with
      | [
       {
         content =
           Ast.Expr.Functor
             {
               name = ([], { content = module_name; _ }) as module_name';
               elements = [];
               arity = 0;
             };
         loc = module_loc;
       };
       {
         content =
           Ast.Expr.Functor { name = signature_name; elements = []; arity = 0 };
         _;
       };
      ] -> (
          let comptime_value = Lookup.ancestors_of_compiler compiler in
          match
            ( Lookup.m0dule comptime_value module_name',
              Lookup.signature comptime_value signature_name )
          with
          | `Ok { loc; _ }, _ | `UnexpectedSignature loc, _ ->
              Logger.error module_loc "Failed to define module";
              Logger.error loc
                "There's already a module or signatures with the same name. \
                 Modules and signatures share their scope.";
              exit 1
          | `Undefined _, `Ok signature ->
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
                    modules =
                      BatMap.String.add module_name compiled_module modules;
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
                "You cannot inspect other signatures when qualifying \
                 signatures. Only modules can be inspected.";
              exit 1)
      | [ { content = _; loc } ] ->
          Logger.error loc "Signature names must be atoms.";
          exit 1
      | _ ->
          Logger.unreachable directive_loc
            "Somehow there is a mismatch between the expected arity and the \
             actual arity.";
          exit 1)
  | "module", 2, _ ->
      Logger.error directive_loc
        "Modules with a named signature must have exactly one body.";
      exit 1
  | "signature", 1, [ body ] -> (
      match elements with
      | [
       {
         content =
           Ast.Expr.Functor
             { name = [], { content = name; _ }; elements = []; arity = 0 };
         loc;
       };
      ] -> (
          match BatMap.String.find_opt name modules with
          | Some existing ->
              Logger.error loc "Failed to define signature";
              Logger.error existing.loc
                "There's already a module or signature with the same name";
              exit 1
          | None ->
              let compiled_sig =
                compiler
                |> Signature.compile directive_loc body
                |> Location.fmap (fun cs -> Signature cs)
              in
              {
                compiler with
                env =
                  {
                    env with
                    modules = BatMap.String.add name compiled_sig modules;
                  };
              })
      | [ { content = _; loc } ] ->
          Logger.error loc "Signature names must be atoms.";
          exit 1
      | _ ->
          Logger.unreachable directive_loc
            "Somehow there is a mismatch between the expected arity and the \
             actual arity.";
          exit 1)
  | "signature", 0, _ ->
      Logger.error directive_loc "Signature directives must have a name.";
      exit 1
  | "signature", n, _ when 1 < n ->
      Logger.error directive_loc
        "Signature directives must have only a name and a body.";
      exit 1
  | "import", 1, [] ->
      Logger.simply_unreachable "TODO: import";
      exit 1
  | "project", 0, _ ->
      Logger.simply_unreachable "TODO";
      exit 1
  | _ ->
      Logger.simply_unreachable "Unknown directive";
      exit 1
