type entry_point = { p_register : int }
type predicate_name = Ast.Clause.head [@@deriving show, ord]

module Form = Beam.Core.Form (Beam.Core.Erlang)
module FT = BatFingerTree
module Set = BatSet

module PredicateMap = BatMap.Make (struct
  type t = predicate_name [@@deriving show, ord]
end)
[@@warning "-32"]

type functor_map = int PredicateMap.t

let show_functor_table (functors : functor_map) : string =
  let open PredicateMap in
  BatSeq.fold_left
    (fun acc ({ Ast.Clause.name; arity }, address) ->
      acc ^ name ^ "/" ^ string_of_int arity ^ ":" ^ string_of_int address
      ^ "\n")
    "" (to_seq functors)

type forms = Form.t FT.t

type ('comptime_entity, 'predicate_definition) module_t = {
  modules : 'comptime_entity Location.with_location BatMap.String.t;
  predicates : 'predicate_definition PredicateMap.t;
  hidden : ('comptime_entity, 'predicate_definition) module_t option;
}

type compiled_signature = {
  modules : signature Location.with_location BatMap.String.t;
  predicates : predicate_name Set.t;
}

and signature =
  | PlainSignature of compiled_signature
  | Abstract (* TODO: add a way to tell different abstract signatures apart *)
  | ModuleSignature of (signature, unit) module_t

type compiled_module = (comptime, Ast.Clause.multi_declaration) module_t
and comptime = Module of compiled_module | Signature of signature

type 'a env = 'a Location.with_location BatMap.String.t

type t = {
  header : forms;
  output : forms;
  filename : string;
  module_name : string;
  parent : t option;
  env : compiled_module;
}

(*
 * Strategy:
 *
 * A file is not necessarily a module.
 *
 * The build procedure must allow you to specify how the files relate
 * to the module system.
 *
 * By default, each file will be wrapped in a module, but the build system
 * may choose to concatenate multiple files into a single module.
 *)

let initialize_nested parent filename module_name : t =
  {
    parent;
    filename;
    module_name;
    header =
      FT.of_list
        [
          Beam.Builder.Attribute.file filename 1;
          (* TODO: this should be a proper atom *)
          Beam.Builder.Attribute.module_ module_name;
        ];
    output = FT.empty;
    env =
      {
        modules = BatMap.String.empty;
        predicates = PredicateMap.empty;
        hidden = None;
      };
  }

let initialize_from_parent module_name parent : t =
  let inner_module_name = parent.module_name ^ "|" ^ module_name in
  let inner_filename =
    (Filename.basename @@ Filename.chop_extension parent.filename)
    ^ "." ^ module_name ^ ".krt"
  in
  initialize_nested (Some parent) inner_filename inner_module_name

let initialize filename : t =
  let module_name = Filename.basename @@ Filename.chop_extension filename in
  initialize_nested None filename module_name

let rec compile_expr (expr : Ast.Expr.t) : Beam.Builder.Expr.t =
  let open Beam in
  match Location.strip_loc expr with
  | Variable var -> Builder.var var
  | Nil -> Builder.nil
  | Cons (h, t) -> Builder.cons (compile_expr h) (compile_expr t)
  | Functor ({ arity; _ } as f) when arity = 0 ->
      Builder.atom @@ Ast.Expr.extract_func_label f
  | Functor ({ elements; _ } as f) ->
      let name = Builder.atom @@ Ast.Expr.extract_func_label f in
      Builder.tuple (name :: List.map compile_expr elements)
  | Integer number -> Builder.int number

let call_with_fresh (name : string) expr =
  let open Beam in
  Ukanren.call_with_fresh @@ Builder.lambda name expr

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

let ascribed_signature_to_module ({ modules; predicates } : compiled_signature)
    : (signature, unit) module_t =
  {
    hidden = None;
    modules;
    predicates =
      predicates
      |> Set.map (fun v -> (v, ()))
      |> Set.to_seq |> PredicateMap.of_seq;
  }

let compile_declaration_bodies
    (clauses : Ast.Clause.decl Location.with_location list) =
  if List.is_empty clauses then (
    Logger.simply_unreachable "Predicates must have at least one body";
    exit 1)
  else
    let open Beam in
    let compile_single_body
        ({ content; _ } : Ast.Clause.decl Location.with_location) :
        Builder.Expr.t =
      let find_variables call = Preprocessor.find_variables (Functor call) in
      let vars =
        content.body
        |> List.map (Fun.compose find_variables Location.strip_loc)
        |> List.fold_left Set.union Set.empty
        |> Set.filter (fun name ->
               Str.string_match (Str.regexp "^[A-Z]") name 0)
      in
      let open Location in
      let body =
        (* TODO: We should use locations when calling Beam helpers. They don't use
           locations yet, hence they are not being sent as arguments *)
        let make_function { content = call; loc } =
          let { Ast.Expr.name; elements; arity } = call in
          let args = List.map compile_expr elements in
          match (name, arity) with
          | ([ { content = "karuta"; _ } ], { content = "eq"; _ }), 2 -> (
              match args with
              | expr1 :: expr2 :: _ -> Ukanren.eq expr1 expr2
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'eq'";
                  exit 1)
          | ([ { content = "karuta"; _ } ], { content = "nat"; _ }), 1 -> (
              match args with
              | expr1 :: _ -> Ukanren.nat expr1
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'nat'";
                  exit 1)
          | _ ->
              (* TODO: handle namespacing *)
              Builder.call
                (Builder.atom @@ Ast.Expr.extract_func_label call)
                args
        in
        content.body |> List.map make_function |> Ukanren.conj
      in
      Set.fold call_with_fresh vars body
    in
    clauses |> List.map compile_single_body |> Ukanren.disj

module Lookup = struct
  type 'a choice =
    [ `NestedLookup of 'a env
    | `UnexpectedSignature of signature Location.with_location ]

  type 'a selector = 'a Location.with_location -> 'a choice

  let (comptime_select : comptime selector) = function
    | { content = Module { modules; _ }; _ } -> `NestedLookup modules
    | { content = Signature signature; loc = sig_loc } ->
        `UnexpectedSignature (Location.add_loc signature sig_loc)

  let (signature_select : signature selector) = function
    | { content = ModuleSignature { modules; _ }; _ } -> `NestedLookup modules
    | { content = (PlainSignature _ | Abstract) as signature; loc = sig_loc } ->
        `UnexpectedSignature (Location.add_loc signature sig_loc)

  let rec lookup_mod_sig (envs : 'a env BatLazyList.t)
      (names : string Location.with_location list) (select : 'a selector) =
    let rec lookup_mod_sig_qualified (rest : string Location.with_location list)
        (value : 'a Location.with_location) =
      match rest with
      | [] -> `Ok value
      | qualifier :: more -> (
          match select value with
          | `NestedLookup modules -> (
              match BatMap.String.find_opt qualifier.content modules with
              | None ->
                  Logger.error qualifier.loc "Undefined qualifier";
                  `Undefined qualifier
              | Some env -> lookup_mod_sig_qualified more env)
          | `UnexpectedSignature signature as unexpected ->
              Logger.error qualifier.loc
                "Qualifiers reference signature instead of module";
              Logger.error signature.loc "Reference is here";
              unexpected)
    in
    match names with
    | [] ->
        Logger.simply_unreachable "";
        exit 1
    | first :: rest -> (
        match Lazy.force envs with
        | BatLazyList.Cons (env, parent) -> (
            match BatMap.String.find_opt first.content env with
            | None -> lookup_mod_sig parent names select
            | Some value -> lookup_mod_sig_qualified rest value)
        | BatLazyList.Nil ->
            Logger.error first.loc "Undefined in current scope";
            `Undefined first)

  (* TODO: Supress log levels to avoid reporting false negatives to the user
     These functions can report their own errors, but they don't know when to exit 1*)

  type scope = comptime Location.with_location BatMap.String.t BatLazyList.t

  let ancestors_of_compiler (compiler : t) : scope =
    let open BatLazyList in
    unfold (Some compiler) (function
      | None -> None
      | Some { parent; env; _ } -> Some (env.modules, parent))

  let signature (scope : scope)
      ((qualifiers, unqualified_name) : Ast.Expr.func_label) =
    match
      lookup_mod_sig scope
        (List.append qualifiers [ unqualified_name ])
        comptime_select
    with
    | `Ok { content = Module m; loc } ->
        Logger.error unqualified_name.loc "Found module instead of signature";
        Logger.error loc "Module defined here";
        `UnexpectedModule (Location.add_loc m loc)
    | `Ok { content = Signature found; loc } -> `Ok (Location.add_loc found loc)
    | (`Undefined _ | `UnexpectedSignature _) as other -> other

  let m0dule (scope : scope)
      ((qualifiers, unqualified_name) : Ast.Expr.func_label) =
    match
      lookup_mod_sig scope
        (List.append qualifiers [ unqualified_name ])
        comptime_select
    with
    | `Ok { content = Module module'; loc } ->
        `Ok (Location.add_loc module' loc)
    | `Ok { content = Signature signature; loc } ->
        Logger.error unqualified_name.loc "Found signature instead of module";
        Logger.error loc "Signature defined here";
        `UnexpectedSignature (Location.add_loc signature loc)
    | (`Undefined _ | `UnexpectedSignature _) as other -> other

  let predicate (scope : scope)
      ((qualifiers, ({ content = name; loc } as name_with_loc)) :
        Ast.Expr.func_label) (arity : int) =
    match lookup_mod_sig scope qualifiers comptime_select with
    | `Ok { content = Module comp_module; _ } -> (
        let open Ast.Clause in
        match PredicateMap.find_opt { name; arity } comp_module.predicates with
        | None ->
            Logger.error loc "Undefined predicate";
            `Undefined name_with_loc
        | Some predicate -> `Ok predicate)
    | `Ok { content = Signature signature; loc = sig_loc } ->
        Logger.error loc "Qualifiers reference signature instead of module";
        Logger.error sig_loc "Reference is here";
        `UnexpectedSignature (Location.add_loc signature sig_loc)
    | (`Undefined _ | `UnexpectedSignature _) as other -> other
end

let rec compile ((body, compiler) : Ast.Clause.t list * t) : Form.t FT.t =
  let { output; header; _ } = compile_step (body, compiler) in
  FT.append header output

and compile_multi_declaration
    (({ name; arity }, first_clause, remaining_clauses) :
      Ast.Clause.head
      * Ast.Clause.decl Location.with_location
      * Ast.Clause.decl Location.with_location list) (compiler : t) : t =
  let declaration =
    let args = List.map string_of_int @@ BatList.range 0 `To (arity - 1) in
    Beam.Builder.single_function_declaration name
      (List.map (fun v -> Beam.Builder.Pattern.Variable v) args)
    @@ compile_declaration_bodies (first_clause :: remaining_clauses)
  in
  let export = Beam.Builder.Attribute.export [ (name, arity) ] in
  {
    compiler with
    output = FT.cons (FT.snoc compiler.output declaration) export;
  }

and compile_module_signature (loc : Location.location)
    (inline_signature : Ast.Clause.t list) (compiler : t) :
    signature Location.with_location =
  let compiled_sig = compile_signature loc inline_signature compiler in
  match compiled_sig with
  | { content = PlainSignature compiled_sig; loc } ->
      Location.add_loc
        (ModuleSignature (ascribed_signature_to_module compiled_sig))
        loc
  | _ ->
      Logger.simply_unreachable
        "Could not compile a module's signature recursively";
      exit 1

and compile_signature (loc : Location.location) (body : Ast.Clause.t list)
    ({ env = { modules; _ } as env; _ } as compiler : t) :
    signature Location.with_location =
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
               (ascribed_signature_to_module
                  { modules = BatMap.String.empty; predicates = Set.empty }))
            next.loc
        in
        let module_of_plain payload =
          Location.add_loc
            (ModuleSignature (ascribed_signature_to_module payload))
            next.loc
        in

        match Location.strip_loc module_signature with
        | Functor { name = label; _ } -> (
            let scope : Lookup.scope = Lookup.ancestors_of_compiler compiler in
            let comptime_value =
              BatMap.String.map
                (Location.fmap (fun s -> Signature s))
                acc.modules
            in
            match
              Lookup.signature (BatLazyList.cons comptime_value scope) label
            with
            | `Ok { content = PlainSignature payload; _ } ->
                signature_happy_case module_name @@ module_of_plain payload
            | `Ok { content = Abstract; _ } ->
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
            let compiled_module_sig =
              compile_module_signature loc inline_signature compiler
            in
            signature_happy_case atom_module_name compiled_module_sig)
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
              } as directive_head;
            _;
          },
          [ directive_body ] ) -> (
        let merge_function _ (lval : comptime Location.with_location option)
            (rval : signature Location.with_location option) =
          match (lval, rval) with
          | Some existing, None -> Some existing
          | None, None -> None
          (* TODO: disallow shadowing everywhere *)
          | _, Some { content; loc } ->
              Some (Location.add_loc (Signature content) loc)
        in
        let nested_compiler =
          compile_directive next.loc directive_head [ directive_body ]
            {
              compiler with
              env =
                {
                  env with
                  modules =
                    BatMap.String.merge merge_function modules acc.modules;
                };
            }
        in
        let signature_name = Ast.Expr.extract_functor_label signature_name in
        match
          BatMap.String.find_opt signature_name nested_compiler.env.modules
        with
        | Some { content = Signature compiled_sig; loc } ->
            signature_happy_case signature_name
            @@ Location.add_loc compiled_sig loc
        | _ ->
            Logger.unreachable next.loc
              "If we reached this point, something is very wrong because \
               compile_directive should have blown up first.";
            exit 1)
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
    PlainSignature
      (List.fold_left step
         { modules = BatMap.String.empty; predicates = Set.empty }
         body)
  in
  Location.add_loc compiled_sig loc

and compile_directive (directive_loc : Location.location)
    ({ elements; arity; _ } as f : Ast.Expr.func)
    (body : Ast.Clause.t list list)
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
          | `Ok { loc; _ } | `UnexpectedSignature { loc; _ } ->
              Logger.error module_loc "Failed to define module";
              Logger.error loc
                "There's already a module or signatures with the same name. \
                 Modules and signatures share their scope.";
              exit 1
          | `Undefined _ ->
              Logger.debug
                "TODO: Deal with matching module implementation with provided \
                 signature";
              let compiled_module =
                ( ( compiler |> initialize_from_parent module_name |> fun c ->
                    compile_step (body, c) )
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
          | `Ok { loc; _ }, _ | `UnexpectedSignature { loc; _ }, _ ->
              Logger.error module_loc "Failed to define module";
              Logger.error loc
                "There's already a module or signatures with the same name. \
                 Modules and signatures share their scope.";
              exit 1
          | `Undefined _, `Ok { content = PlainSignature _signature; _ } ->
              Logger.debug
                "TODO: Deal with matching module implementation with provided \
                 signature";
              let compiled_module =
                ( ( compiler |> initialize_from_parent module_name |> fun c ->
                    compile_step (body, c) )
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
              }
          | `Undefined _, `Ok { content = Abstract; _ } ->
              Logger.unreachable directive_loc
                "Found signature for module cannot be abstract";
              exit 1
          | `Undefined _, `Ok { content = ModuleSignature _; _ } ->
              Logger.unreachable directive_loc
                "Found signature for module cannot be a module signature \
                 definition";
              exit 1
          | `Undefined _, `UnexpectedModule { loc; _ } ->
              Logger.error loc "Modules are not signatures";
              exit 1
          | `Undefined _, `Undefined { loc; _ } ->
              Logger.error loc
                "This name is undefined when looking up for signatures";
              exit 1
          | `Undefined _, `UnexpectedSignature { loc; _ } ->
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
                compile_signature directive_loc body compiler
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

and compile_clause (clause : Ast.Clause.t) (compiler : t) : t =
  (* TODO: handle location *)
  match clause.content with
  | Directive ({ loc; content = { name = _ :: _, _; _ } }, _) ->
      Logger.error loc "Directives cannot be qualified";
      exit 1
  | Directive ({ content = header; loc }, body) ->
      print_endline "Directives for some reason";
      compile_directive loc header body compiler
  | MultiDeclaration (header, first, rest) ->
      let open Location in
      compile_multi_declaration
        (header, { content = first; loc = clause.loc }, rest)
        compiler
  | Query { name; arity; args } ->
      let open Beam in
      let declaration =
        let fun_args =
          List.init (arity + 1) @@ Fun.const Builder.pattern_wildcard
        in
        args |> List.map Builder.var
        |> Builder.call (Builder.atom "")
        |> List.fold_right Ukanren.query_variable args
        |> List.fold_right call_with_fresh args
        |> Ukanren.run_lazy
        |> Builder.single_function_declaration name fun_args
      in
      let export = Beam.Builder.Attribute.export [ (name, arity + 1) ] in
      {
        compiler with
        output = FT.cons (FT.snoc compiler.output declaration) export;
      }

(* TODO: figure out the logistics of handling the runtime lib *)
and compile_step : Ast.Clause.t list * t -> t = function
  | [], compiler ->
      let list_forms = FT.to_list (FT.append compiler.header compiler.output) in
      let open Beam.Serializer in
      let forms =
        "["
        ^ (String.concat "," @@ List.map Attribute.to_string list_forms)
        ^ "]"
      in
      print_endline forms;
      compiler
  | clause :: remaining, compiler ->
      (* TODO: We should save the intermediary outputs, due to this being a step by module *)
      compile_step (remaining, compile_clause clause compiler)
