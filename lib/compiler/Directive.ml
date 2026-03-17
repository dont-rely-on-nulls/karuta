open Types

let rec ascribe_signature
    ({ Location.content = given_signature; loc = sig_loc } :
      signature Location.with_location)
    ({ Location.content = given_module; loc = module_loc } :
      compiled_module Location.with_location) :
    compiled_module Location.with_location =
  match given_signature with
  | ModuleSignature given_signature | PlainSignature given_signature ->
      let public_predicates, hidden_predicates =
        PredicateMap.partition
          (fun pred_name _ -> BatSet.mem pred_name given_signature.predicates)
          given_module.predicates
      in
      (* TODO: The functionality below can be separated into a function for both predicates and comptimes *)
      (* TODO: We are using sets, which do not have order in consideration. However,
         this matters for listing the missing ones. One would expect the order of reported
         missing ones to be in order of declaration in the signature. You can either change
         the type to be a finger tree OR include location into those predicate names.*)
      let module_predicate_names =
        BatSet.of_enum @@ PredicateMap.keys public_predicates
      in
      let missing_predicates =
        BatSet.diff given_signature.predicates module_predicate_names
      in
      if not @@ BatSet.is_empty missing_predicates then (
        let first, rest =
          missing_predicates |> BatSet.to_seq |> Seq.uncons |> function
          | Some elems -> elems
          | None ->
              Logger.simply_unreachable
                "The above if is guarding against this case";
              exit 1
        in
        let make_msg ({ name; arity } : predicate_name) : string =
          name ^ "/" ^ string_of_int arity
        in
        let error_msg =
          Seq.fold_left
            (fun acc missing_predicate ->
              acc ^ ", " ^ make_msg missing_predicate)
            (make_msg first) rest
        in
        Logger.error module_loc
          "Mismatch between signature and module: some required predicates are \
           missing";
        Logger.error sig_loc "Signature defined here";
        Logger.simply_error
        @@ "The following predicates are missing implementation: " ^ error_msg;
        exit 1);
      let public_comptimes, hidden_comptimes =
        BatMap.String.partition
          (fun comptime_name _ ->
            BatMap.String.mem comptime_name given_signature.modules)
          given_module.modules
      in
      let public_comptimes_set =
        BatSet.of_enum @@ BatMap.String.keys public_comptimes
      in
      let signature_comptimes_set =
        BatSet.of_enum @@ BatMap.String.keys given_signature.modules
      in
      let missing_comptimes =
        BatSet.diff signature_comptimes_set public_comptimes_set
      in
      if not @@ BatSet.is_empty missing_comptimes then (
        let first, rest =
          missing_comptimes |> BatSet.to_seq |> Seq.uncons |> function
          | Some elems -> elems
          | None ->
              Logger.simply_unreachable
                "The above if is guarding against this case";
              exit 1
        in
        let error_msg =
          Seq.fold_left
            (fun acc missing_comptime -> acc ^ ", " ^ missing_comptime)
            first rest
        in
        Logger.error module_loc
          "Mismatch between signature and module: some required modules or \
           signatures are missing";
        Logger.error sig_loc "Signature defined here";
        Logger.simply_error @@ "The following are missing implementation: "
        ^ error_msg;
        exit 1);
      let public_comptimes : comptime Location.with_location BatMap.String.t =
        BatMap.String.mapi
          (fun k (v : comptime Location.with_location) ->
            match BatMap.String.find_opt k given_signature.modules with
            | None ->
                Logger.simply_unreachable
                  "Every key in public_comptimes must be in \
                   given_signature.modules at this point.";
                exit 1
            | Some nested_sig -> (
                match v.content with
                | Signature module_signature -> (
                    match nested_sig.content with
                    | PlainSignature compiled_signature ->
                        (* TODO: You must check for predicates as well otherwise nested stuff won't be checked *)
                        let module_keys =
                          BatSet.of_enum
                          @@ BatMap.String.keys module_signature.modules
                        in
                        let sig_keys =
                          BatSet.of_enum
                          @@ BatMap.String.keys compiled_signature.modules
                        in
                        if BatSet.equal module_keys sig_keys then v
                        else (
                          Logger.simply_warning
                            "Signature subtyping is not implemented yet";
                          Logger.error v.loc
                            "Nested signatures cannot differ from implemented \
                             ones";
                          Logger.error nested_sig.loc "Signature here";
                          exit 1)
                    | ModuleSignature _ ->
                        Logger.error nested_sig.loc
                          "Signature mandates this to be a module";
                        Logger.error v.loc
                          "Found a signature in module implementation";
                        exit 1
                    | Abstract _ ->
                        Logger.unreachable v.loc
                          "Abstract signatures and modules are not implemented \
                           yet";
                        exit 1)
                | Module nested_module ->
                    Location.fmap (fun m -> Module m)
                    @@ ascribe_signature nested_sig
                         (Location.add_loc nested_module v.loc)))
          public_comptimes
      in
      Location.add_loc
        {
          modules = public_comptimes;
          predicates = public_predicates;
          hidden =
            Some { modules = hidden_comptimes; predicates = hidden_predicates };
        }
        module_loc
  | Abstract _ ->
      Logger.simply_unreachable
        "Abstract signatures and modules are not implemented yet";
      exit 1

let initialize_from_parent module_name parent : t =
  let inner_module_name =
    parent.module_name ^ Common.module_name_separator ^ module_name
  in
  let inner_filename =
    (Filename.basename @@ Filename.chop_extension parent.filename)
    ^ "." ^ module_name ^ ".krt"
  in
  initialize_nested parent.persist (Some parent) inner_filename
    inner_module_name

let rec compile (directive_loc : Location.location)
    ({ elements; arity; _ } as f : Ast.Expr.func)
    (body : Ast.Clause.t list list) (step : Ast.Clause.t list * t -> t)
    ({ env = { modules; _ } as env; _ } as compiler : t) : t =
  let _, _, _ = (elements, body, compiler) in
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
                ( ( compiler |> initialize_from_parent module_name |> fun c ->
                    step (body, c) )
                |> fun c -> c.env )
                |> fun e -> Location.add_loc (Module e) directive_loc
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
        compile directive_loc f [ body ] step compiler
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
                        (Location.fmap
                           (fun m -> Module m)
                           (ascribe_signature
                              (Location.fmap
                                 (fun s -> PlainSignature s)
                                 inline_sig)
                              content))
                        modules;
                  };
              }
          | _ -> failwith "TODO")
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
                ( ( compiler |> initialize_from_parent module_name |> fun c ->
                    step (body, c) )
                |> fun c -> c.env )
                |> fun e -> Location.add_loc e directive_loc
              in
              let compiled_module =
                Location.fmap
                  (fun v -> Module v)
                  (ascribe_signature
                     (Location.fmap (fun v -> PlainSignature v) signature)
                     comptime)
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
                Signature.compile directive_loc body compiler
              in
              let compiled_sig =
                Location.add_loc (Signature compiled_sig.content)
                  compiled_sig.loc
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
  | "project", 0, _ ->
      Logger.simply_unreachable "TODO";
      exit 1
  | _ ->
      Logger.simply_unreachable "Unknown directive";
      exit 1
