open Compiler

let sig_env_cons :
    sig_env Location.with_location ->
    compiled_signature ->
    sig_env Location.with_location =
 fun lhs rhs ->
  Location.fmap
    (fun lhs -> BatMap.String.union (fun _ _ rhs -> Some rhs) lhs rhs.modules)
    lhs

module Diff : sig
  val predicates :
    predicate_name BatSet.t ->
    predicate_name BatSet.t ->
    Location.location ->
    Location.location ->
    unit

  val comptimes :
    comptime env -> sig_env -> Location.location -> Location.location -> unit
end = struct
  let when_populated entities f =
    match entities |> BatSet.to_seq |> Seq.uncons with
    | None -> ()
    | Some elems -> f elems

  let predicates (provided : predicate_name BatSet.t)
      (required : predicate_name BatSet.t) (module_loc : Location.location)
      (sig_loc : Location.location) : unit =
    when_populated (BatSet.diff required provided) @@ fun (first, rest) ->
    let make_msg ({ name; arity } : predicate_name) : string =
      name ^ "/" ^ string_of_int arity
    in
    let error_msg =
      Seq.fold_left
        (fun acc missing_predicate -> acc ^ ", " ^ make_msg missing_predicate)
        (make_msg first) rest
    in
    Logger.error module_loc
      "Mismatch between signature and module: some required predicates are \
       missing";
    Logger.error sig_loc "Signature defined here";
    Logger.simply_error
    @@ "The following predicates are missing implementation: " ^ error_msg;
    exit 1

  let comptimes (provided : comptime env) (required : sig_env)
      (module_loc : Location.location) (sig_loc : Location.location) : unit =
    let provided_comptimes_set =
      BatSet.of_enum @@ BatMap.String.keys provided
    in
    let required_comptimes_set =
      BatSet.of_enum @@ BatMap.String.keys required
    in
    let missing_comptimes =
      BatSet.diff required_comptimes_set provided_comptimes_set
    in
    when_populated missing_comptimes @@ fun (first, rest) ->
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
    exit 1
end

let rec ascribe_to_module
    ({ content = given_module; loc = module_loc } :
      compiled_module Location.with_location)
    ({ content = given_signature; loc = sig_loc } :
      compiled_signature Location.with_location) :
    compiled_module Location.with_location =
  let public_predicates =
    PredicateMap.filter
      (fun pred_name _ -> BatSet.mem pred_name given_signature.predicates)
      given_module.predicates
  in
  (* TODO: https://github.com/dont-rely-on-nulls/karuta/issues/34 *)
  let public_predicates_names =
    BatSet.of_enum @@ PredicateMap.keys public_predicates
  in
  Diff.predicates public_predicates_names given_signature.predicates module_loc
    sig_loc;
  let public_comptimes =
    BatMap.String.filter
      (fun comptime_name _ ->
        BatMap.String.mem comptime_name given_signature.modules)
      given_module.modules
  in
  Diff.comptimes public_comptimes given_signature.modules module_loc sig_loc;
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
            match (v.content, nested_sig.content) with
            | Signature module_signature, (PlainSignature _ as rhs) ->
                if signature_equal (PlainSignature module_signature) rhs then v
                else (
                  Logger.simply_warning
                    "Signature subtyping is not implemented yet";
                  Logger.error v.loc
                    "Nested signatures cannot differ from implemented ones";
                  Logger.error nested_sig.loc "Signature here";
                  exit 1)
            | Signature _, ModuleSignature _ ->
                Logger.error nested_sig.loc
                  "Signature mandates this to be a module";
                Logger.error v.loc "Found a signature in module implementation";
                exit 1
            | (Signature _ | Module _), Abstract _ ->
                Logger.unreachable v.loc
                  "Abstract signatures and modules are not implemented yet";
                exit 1
            | Module _, PlainSignature _ ->
                Logger.error nested_sig.loc
                  "Signature mandates this to be a signature";
                Logger.error v.loc "Found a module instead";
                exit 1
            | Module nested_module, ModuleSignature nested_mod_sig ->
                Location.fmap (fun m -> Module m)
                @@ ascribe_to_module
                     (Location.add_loc nested_module v.loc)
                     (Location.add_loc nested_mod_sig nested_sig.loc)))
      public_comptimes
  in
  Location.add_loc
    {
      qualifier = given_module.qualifier;
      modules = public_comptimes;
      predicates = public_predicates;
      query = None;
    }
    module_loc

let rec compile_nested : type a mods directive.
    Location.location ->
    (directive, mods) Ast.Module.signature_body ->
    a t ->
    sig_env Location.with_location ->
    compiled_signature Location.with_location =
 fun loc body ({ env = { modules; _ }; _ } as compiler) sig_env ->
  let module Lookup = (val compiler.lookup) in
  let directive_step (acc : compiled_signature)
      (next : (directive, mods) Ast.Module.directive Location.with_location) =
    let happy_case (comptime_name : string)
        (definition : signature Location.with_location) =
      {
        acc with
        modules = BatMap.String.add comptime_name definition acc.modules;
      }
    in
    match next.content with
    | Module
        {
          name = { content = module_name; _ };
          signature =
            Some
              { content = Named module_signature; loc = module_signature_loc };
          directives;
          declarations;
          _;
        }
      when FT.is_empty directives && BatMap.is_empty declarations -> (
        let report_module_as_signature sig_loc module_loc =
          Logger.error sig_loc
            "This name does not actually refer to a signature.";
          Logger.error module_loc "Definition in scope.";
          exit 1
        in
        let module_of_abstract =
          (* TODO: handle this correctly *)
          Location.add_loc
            (ModuleSignature
               { modules = BatMap.String.empty; predicates = Set.empty })
            next.loc
        in
        let module_of_plain payload =
          Location.add_loc (ModuleSignature payload) next.loc
        in
        match
          Lookup.nested_signature (sig_env_cons sig_env acc)
            (Lookup.comptime_of_compiler compiler)
            module_signature
        with
        | `Ok { content = PlainSignature payload; _ } ->
            happy_case module_name @@ module_of_plain payload
        | `Ok { content = Abstract _; _ } ->
            happy_case module_name module_of_abstract
        | `UnexpectedModule { loc = outer; _ }
        | `Ok { content = ModuleSignature _; loc = outer } ->
            report_module_as_signature module_signature_loc outer
        | `UnexpectedSignature _ -> exit 1
        | `Undefined _ ->
            Logger.error module_signature_loc
              "Undefined signature name. Remember: the order matters (for now \
               😉).";
            exit 1)
    | Module
        {
          name = { content = atom_module_name; _ };
          signature = Some { content = Inlined inline_signature; loc = sig_loc };
          directives;
          declarations;
          _;
        }
      when FT.is_empty directives
           && BatMap.is_empty declarations
           && Ast.Module.signature_populated inline_signature -> (
        match BatMap.String.find_opt atom_module_name modules with
        | Some existing ->
            Logger.error loc "Failed to define module within a signature";
            Logger.error existing.loc
              "There's already a module or signature with the same name";
            exit 1
        | None ->
            let compiled_module_sig =
              compile_nested sig_loc inline_signature compiler
              @@ sig_env_cons sig_env acc
            in
            happy_case atom_module_name
              (Location.fmap (fun m -> ModuleSignature m) compiled_module_sig))
    | Signature { name = { content = signature_name; _ }; body } ->
        let compiled_sig =
          compile_nested next.loc body compiler @@ sig_env_cons sig_env acc
        in
        happy_case signature_name
        @@ Location.fmap (fun v -> PlainSignature v) compiled_sig
    | Module
        {
          signature =
            Some
              { content = Inlined { declarations; directives }; loc = sig_loc };
          _;
        }
      when FT.is_empty directives && BatMap.is_empty declarations ->
        Logger.error sig_loc
          "This signature turns your module into a giant unit. Did you forget \
           to add a body?";
        exit 1
    | Module { signature = None; _ } ->
        Logger.error next.loc
          "Module declarations in signatures must have a signature.";
        exit 1
    | Module { directives; declarations; _ }
      when (not (FT.is_empty directives)) || not (BatMap.is_empty declarations)
      ->
        Logger.error next.loc
          "Module declarations in signatures cannot have a body.";
        exit 1
    | Module { signature = Some _; _ } ->
        Logger.simply_unreachable
          "The compiler should have handled all possible combinations for \
           modules inside signatures at this point.";
        exit 1
    | TargetSpecific _ ->
        Logger.simply_unreachable
          "Target specific directive should not be handled here";
        exit 1
  in
  Location.add_loc
    (FT.fold_left directive_step
       {
         modules = BatMap.String.empty;
         predicates =
           BatEnum.fold (Fun.flip Set.add) Set.empty
             (BatMap.keys body.declarations);
       }
       body.directives)
    loc

and compile : type a mods directive.
    Location.location ->
    (directive, mods) Ast.Module.signature_body ->
    a t ->
    compiled_signature Location.with_location =
 fun loc body compiler ->
  compile_nested loc body compiler
  @@ Location.add_loc BatMap.String.empty Location.dummy
