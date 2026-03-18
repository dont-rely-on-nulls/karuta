open Types

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
  let get_first_rest (entities : 'a BatSet.t) : 'a * 'a Seq.t =
    entities |> BatSet.to_seq |> Seq.uncons |> function
    | Some elems -> elems
    | None ->
        Logger.simply_unreachable
          "This is an invariant about missing either predicates or comptime \
           entities";
        exit 1

  let predicates (provided : predicate_name BatSet.t)
      (required : predicate_name BatSet.t) (module_loc : Location.location)
      (sig_loc : Location.location) : unit =
    let missing_names = BatSet.diff required provided in
    if not @@ BatSet.is_empty missing_names then (
      let first, rest = get_first_rest missing_names in
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
      exit 1)

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
    if not @@ BatSet.is_empty missing_comptimes then (
      let first, rest = get_first_rest missing_comptimes in
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
      exit 1)
end

let rec ascribe_to_module
    ({ Location.content = given_module; loc = module_loc } :
      compiled_module Location.with_location)
    ({ Location.content = given_signature; loc = sig_loc } :
      signature Location.with_location) : compiled_module Location.with_location
    =
  match given_signature with
  | ModuleSignature given_signature | PlainSignature given_signature ->
      let public_predicates, hidden_predicates =
        PredicateMap.partition
          (fun pred_name _ -> BatSet.mem pred_name given_signature.predicates)
          given_module.predicates
      in
      (* TODO: We are using sets, which do not have order in consideration. However,
         this matters for listing the missing ones. One would expect the order of reported
         missing ones to be in order of declaration in the signature. You can either change
         the type to be a finger tree OR include location into those predicate names.*)
      let public_predicates_names =
        BatSet.of_enum @@ PredicateMap.keys public_predicates
      in
      Diff.predicates public_predicates_names given_signature.predicates
        module_loc sig_loc;
      let public_comptimes, hidden_comptimes =
        BatMap.String.partition
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
                match v.content with
                | Signature module_signature -> (
                    match nested_sig.content with
                    | PlainSignature compiled_signature ->
                        Diff.predicates module_signature.predicates
                          compiled_signature.predicates v.loc nested_sig.loc;
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
                    @@ ascribe_to_module
                         (Location.add_loc nested_module v.loc)
                         nested_sig))
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

let all_atoms (args : Ast.Expr.t list) : unit =
  match
    args
    |> List.find_opt
       @@ Fun.compose
            (function
              | Ast.Expr.Functor { arity = 0; elements = []; _ } -> false
              | _ -> true)
            Location.strip_loc
  with
  | None -> ()
  | Some { loc; _ } ->
      Logger.error loc "Expected atom.";
      exit 1

let rec compile_nested (loc : Location.location) (body : Ast.Clause.t list)
    ({ env = { modules; _ }; _ } as compiler : t) (sig_scope : Lookup.sig_scope)
    =
  let all_underscores : Ast.Expr.t list -> bool =
    List.for_all
    @@ Fun.compose
         (function Ast.Expr.Variable "_" -> true | _ -> false)
         Location.strip_loc
  in
  let step (acc : compiled_signature) (next : Ast.Clause.t) =
    let predicate_happy_case (head : predicate_name) =
      { acc with predicates = Set.add head acc.predicates }
    in
    let signature_happy_case (comptime_name : string)
        (definition : signature Location.with_location) =
      {
        acc with
        modules = BatMap.String.add comptime_name definition acc.modules;
      }
    in
    match next.content with
    | MultiDeclaration (_, _, second :: (_ as remaining_bodies)) ->
        Logger.error next.loc
          "You cannot have multiple definitions when declaring a predicate in \
           a signature.";
        Logger.error second.loc "Second definition here.";
        (match remaining_bodies with
        | [] -> ()
        | more ->
            Logger.simply_error @@ "Plus "
            ^ (string_of_int @@ List.length more)
            ^ " other definitions.");
        exit 1
    | MultiDeclaration (head, { original_arg_list; body }, [])
      when List.length body = head.arity && all_underscores original_arg_list ->
        predicate_happy_case head
    | MultiDeclaration (head, { body; _ }, [])
      when List.length body = head.arity ->
        Logger.warning next.loc
          "Types are not supported yet. Ignoring argument types.";
        predicate_happy_case head
    | MultiDeclaration _ ->
        Logger.error next.loc
          "You cannot have a body when declaring a predicate in a signature.";
        exit 1
    | Directive
        ( {
            content =
              {
                name = [], { content = "module"; _ };
                arity = 2;
                elements = [ module_name; module_signature ] as elements;
              };
            _;
          },
          [] ) -> (
        all_atoms elements;
        let module_name = Ast.Expr.extract_unqualified_atom module_name in
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

        match Location.strip_loc module_signature with
        | Functor { name = label; _ } -> (
            let scope : Lookup.scope = Lookup.ancestors_of_compiler compiler in
            match
              Lookup.nested_signature
                (Lookup.sig_env_to_sig_scope acc.modules)
                scope label
            with
            | `Ok { content = PlainSignature payload; _ } ->
                signature_happy_case module_name @@ module_of_plain payload
            | `Ok { content = Abstract _; _ } ->
                signature_happy_case module_name module_of_abstract
            | `UnexpectedModule { loc = outer; _ }
            | `Ok { content = ModuleSignature _; loc = outer } ->
                report_module_as_signature module_signature.loc outer
            | `UnexpectedSignature _ -> exit 1
            | `Undefined _ ->
                Logger.error module_signature.loc
                  "Undefined signature name. Remember: the order matters (for \
                   now 😉).";
                exit 1)
        | _ ->
            Logger.unreachable next.loc
              "Inconsistency between all_atoms and pattern match";
            exit 1)
    | Directive
        ( {
            content =
              {
                name = [], { content = "module"; _ };
                arity = 1;
                elements = [ module_name ];
              };
            _;
          },
          [ inline_signature ] )
      when inline_signature != [] -> (
        all_atoms [ module_name ];
        let atom_module_name = Ast.Expr.extract_functor_label module_name in
        match BatMap.String.find_opt atom_module_name modules with
        | Some existing ->
            Logger.error loc "Failed to define module within a signature";
            Logger.error existing.loc
              "There's already a module or signature with the same name";
            exit 1
        | None ->
            let compiled_module_sig = compile loc inline_signature compiler in
            signature_happy_case atom_module_name
              (Location.fmap (fun m -> ModuleSignature m) compiled_module_sig))
    | Directive ({ content = { name = [], { content = "module"; _ }; _ }; _ }, _)
      ->
        Logger.error next.loc
          "Module declarations in signatures must have exactly one name and \
           one signature.";
        exit 1
    | Directive
        ( {
            content =
              {
                name = [], { content = "signature"; _ };
                elements = signature_name :: _;
                _;
              };
            _;
          },
          [ directive_body ] ) ->
        let compiled_sig =
          compile_nested next.loc directive_body compiler
            (Lookup.sig_cons sig_scope acc.modules)
        in
        let signature_name = Ast.Expr.extract_functor_label signature_name in
        signature_happy_case signature_name
        @@ Location.fmap (fun v -> PlainSignature v) compiled_sig
    | Directive
        ( {
            content =
              { name = [], { content = "signature"; _ }; elements = _ :: _; _ };
            _;
          },
          _ :: _ :: _ ) ->
        Logger.error loc "Signatures must have at most one body";
        exit 1
    | Directive ({ loc; content = { name = _ :: _, _; _ } }, _) ->
        Logger.error loc "Directives in signatures cannot be qualified";
        exit 1
    | def ->
        Logger.debug @@ Ast.Clause.show_base def;
        acc
  in
  let compiled_sig =
    List.fold_left step
      { modules = BatMap.String.empty; predicates = Set.empty }
      body
  in
  Location.add_loc compiled_sig loc

and compile (loc : Location.location) (body : Ast.Clause.t list) (compiler : t)
    : compiled_signature Location.with_location =
  compile_nested loc body compiler Lookup.empty_signature
