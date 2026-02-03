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
}

type compiled_signature = {
  modules : signature Location.with_location BatMap.String.t;
  predicates : predicate_name Set.t;
}

and signature =
  | PlainSignature of compiled_signature
  | Abstract (* TODO: add a way to tell different abstract signatures apart *)
  | ModuleSignature of (signature, unit) module_t

type comptime =
  | Module of (comptime, Ast.Clause.multi_declaration) module_t
  | Signature of signature

type t = {
  header : forms;
  output : forms;
  env : (comptime, Ast.Clause.multi_declaration) module_t;
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

let initialize_nested filename module_name : t =
  {
    header =
      FT.of_list
        [
          Beam.Builder.Attribute.file filename 1;
          (* TODO: this should be a proper atom *)
          Beam.Builder.Attribute.module_ module_name;
        ];
    output = FT.empty;
    env = { modules = BatMap.String.empty; predicates = PredicateMap.empty };
  }

let initialize filename : t =
  let module_name = Filename.basename @@ Filename.chop_extension filename in
  initialize_nested filename module_name

let rec compile_expr (expr : Ast.Expr.t) : Beam.Builder.Expr.t =
  let open Beam in
  match Location.strip_loc expr with
  | Variable var -> Builder.var var
  | Nil -> Builder.nil
  | Cons (h, t) -> Builder.cons (compile_expr h) (compile_expr t)
  | Functor { name; arity; _ } when arity = 0 -> Builder.atom name
  | Functor { name; elements; _ } ->
      let name = Builder.atom name in
      Builder.tuple (name :: List.map compile_expr elements)
  | Integer number -> Builder.int number

let call_with_fresh (name : string) expr =
  let open Beam in
  Ukanren.call_with_fresh @@ Builder.lambda name expr

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
      let find_variables call =
        Preprocessor.find_variables (Functor (Preprocessor.func_of_call call))
      in
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
          let { Ast.Expr.name; elements; arity } =
            Preprocessor.func_of_call call
          in
          let args = List.map compile_expr elements in
          match (name, arity) with
          | "eq", 2 -> (
              match args with
              | expr1 :: expr2 :: _ -> Ukanren.eq expr1 expr2
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'eq'";
                  exit 1)
          | "nat", 1 -> (
              match args with
              | expr1 :: _ -> Ukanren.nat expr1
              | _ ->
                  Logger.unreachable loc
                    "Mismatch between arity and length of elements in builtin \
                     'nat'";
                  exit 1)
          | _ -> Builder.call (Builder.atom name) args
        in
        content.body |> List.map make_function |> Ukanren.conj
      in
      Set.fold call_with_fresh vars body
    in
    clauses |> List.map compile_single_body |> Ukanren.disj

let compile_multi_declaration
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
    modules;
    predicates =
      predicates
      |> Set.map (fun v -> (v, ()))
      |> Set.to_seq |> PredicateMap.of_seq;
  }

let rec compile_module_signature (loc : Location.location)
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
            name = "module";
            arity = 2;
            elements = [ module_name; module_signature ] as elements;
          },
          [] ) -> (
        all_atoms elements;
        let report_module_as_signature sig_loc module_loc =
          Logger.error sig_loc
            "This name does not actually refer to a signature.";
          Logger.error module_loc "Definition in scope.";
          exit 1
        in
        match
          (Location.strip_loc module_name, Location.strip_loc module_signature)
        with
        | ( Functor { name = module_name'; _ },
            Functor { name = module_signature'; _ } ) -> (
            match BatMap.String.find_opt module_signature' acc.modules with
            | Some { content = PlainSignature payload; _ } ->
                signature_happy_case module_name'
                @@ Location.add_loc
                     (ModuleSignature (ascribed_signature_to_module payload))
                     next.loc
            | Some { content = Abstract; _ } ->
                (* TODO: handle this correctly *)
                signature_happy_case module_name'
                @@ Location.add_loc
                     (ModuleSignature
                        (ascribed_signature_to_module
                           {
                             modules = BatMap.String.empty;
                             predicates = Set.empty;
                           }))
                     next.loc
            | Some { content = ModuleSignature _; loc } ->
                report_module_as_signature next.loc loc
            | None -> (
                match BatMap.String.find_opt module_signature' env.modules with
                | Some
                    { content = Signature (PlainSignature _ as outer_sig); loc }
                | Some { content = Signature (Abstract as outer_sig); loc } ->
                    signature_happy_case module_name'
                    @@ Location.add_loc outer_sig loc
                | Some { loc = outer; content = Signature (ModuleSignature _) }
                | Some { loc = outer; content = Module _ } ->
                    report_module_as_signature module_signature.loc outer
                | None ->
                    Logger.error module_signature.loc
                      "Undefined signature name. Remember: the order matters \
                       (for now 😉).";
                    exit 1))
        | _ ->
            Logger.unreachable next.loc
              "Inconsistency between all_atoms and pattern match";
            exit 1)
    | Directive
        ( { name = "module"; arity = 1; elements = [ module_name ] },
          inline_signature )
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
    | Directive ({ name = "module"; _ }, _) ->
        Logger.error next.loc
          "Module declarations in signatures must have exactly one name and \
           one signature.";
        exit 1
    | Directive
        ( ({ name = "signature"; elements = signature_name :: _; _ } as
           directive_head),
          directive_body ) -> (
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
          compile_directive next.loc directive_head directive_body
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
    ({ name; elements; arity } : Ast.Expr.func) (body : Ast.Clause.t list)
    ({ env = { modules; _ } as env; _ } as compiler : t) : t =
  let _, _, _ = (elements, body, compiler) in
  match (name, arity) with
  | "module", 1 ->
      Logger.simply_unreachable "TODO";
      exit 1
  | "module", 2 ->
      Logger.simply_unreachable "TODO";
      exit 1
  | "signature", 1 -> (
      match elements with
      | [
       { content = Ast.Expr.Functor { name; elements = []; arity = 0 }; loc };
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
  | "signature", 0 ->
      Logger.error directive_loc "Signature directives must have a name.";
      exit 1
  | "signature", n when 1 < n ->
      Logger.error directive_loc
        "Signature directives must have only a name and a body.";
      exit 1
  | "project", 0 ->
      Logger.simply_unreachable "TODO";
      exit 1
  | _ ->
      Logger.simply_unreachable "Unknown directive";
      exit 1

let compile_clause (clause : Ast.Clause.t) (compiler : t) : t =
  (* TODO: handle location *)
  match clause.content with
  | Directive (header, body) ->
      compile_directive clause.loc header body compiler
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
let rec compile : Ast.Clause.t list * t -> Form.t FT.t = function
  | [], { output; header; _ } -> FT.append header output
  | clause :: remaining, compiler ->
      compile (remaining, compile_clause clause compiler)
