open Types

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
      print_endline @@ "Happy case: " ^ show_predicate_name head;
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
