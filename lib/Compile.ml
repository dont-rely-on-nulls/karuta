open Compiler.Types

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

let flat_module_name (path : string list) =
  let concat_segments l r = l ^ Compiler.Common.module_name_separator ^ r in
  match path with
  | [] -> ""
  | head :: tail -> List.fold_left concat_segments head tail

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

let compile_declaration_bodies module_name
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
          | ([], _), _ ->
              Builder.call
                (Builder.atom @@ Ast.Expr.extract_func_label call)
                args
          | ((_ :: _ as path), { content = fun_name; _ }), _ ->
              Builder.call_with_module
                (Builder.atom
                @@ flat_module_name
                     (module_name :: List.map Location.strip_loc path))
                (Builder.atom fun_name) args
        in

        content.body |> List.map make_function |> Ukanren.conj
      in
      Set.fold call_with_fresh vars body
    in
    clauses |> List.map compile_single_body |> Ukanren.disj

(* module Lookup = struct *)
(*   type 'a choice = *)
(*     [ `NestedLookup of 'a env | `UnexpectedSignature of Location.location ] *)

(*   type 'a selector = 'a Location.with_location -> 'a choice *)

(*   let (comptime_select : comptime selector) = function *)
(*     | { content = Module { modules; _ }; _ } -> `NestedLookup modules *)
(*     | { content = Signature _; loc = sig_loc } -> `UnexpectedSignature sig_loc *)

(*   let (signature_select : signature selector) = function *)
(*     | { content = ModuleSignature { modules; _ }; _ } -> `NestedLookup modules *)
(*     | { content = PlainSignature _ | Abstract _; loc = sig_loc } -> *)
(*         `UnexpectedSignature sig_loc *)

(*   let rec lookup_mod_sig (envs : 'a env BatLazyList.t) *)
(*       (names : string Location.with_location list) (select : 'a selector) = *)
(*     let rec lookup_mod_sig_qualified (rest : string Location.with_location list) *)
(*         (value : 'a Location.with_location) = *)
(*       match rest with *)
(*       | [] -> `Ok value *)
(*       | qualifier :: more -> ( *)
(*           match select value with *)
(*           | `NestedLookup modules -> ( *)
(*               match BatMap.String.find_opt qualifier.content modules with *)
(*               | None -> *)
(*                   Logger.error qualifier.loc "Undefined qualifier"; *)
(*                   `Undefined qualifier *)
(*               | Some env -> lookup_mod_sig_qualified more env) *)
(*           | `UnexpectedSignature sig_loc as unexpected -> *)
(*               Logger.error qualifier.loc *)
(*                 "Qualifiers reference signature instead of module"; *)
(*               Logger.error sig_loc "Reference is here"; *)
(*               unexpected) *)
(*     in *)
(*     match names with *)
(*     | [] -> *)
(*         Logger.simply_unreachable ""; *)
(*         exit 1 *)
(*     | first :: rest -> ( *)
(*         match Lazy.force envs with *)
(*         | BatLazyList.Cons (env, parent) -> ( *)
(*             match BatMap.String.find_opt first.content env with *)
(*             | None -> lookup_mod_sig parent names select *)
(*             | Some value -> lookup_mod_sig_qualified rest value) *)
(*         | BatLazyList.Nil -> *)
(*             Logger.error first.loc "Undefined in current scope"; *)
(*             `Undefined first) *)

(*   (\* TODO: Supress log levels to avoid reporting false negatives to the user *)
(*      These functions can report their own errors, but they don't know when to exit 1*\) *)

(*   type 'a nested_env = 'a Location.with_location BatMap.String.t BatLazyList.t *)
(*   type scope = comptime nested_env *)
(*   type sig_scope = signature nested_env *)

(*   let ancestors_of_compiler (compiler : t) : scope = *)
(*     let open BatLazyList in *)
(*     unfold (Some compiler) (function *)
(*       | None -> None *)
(*       | Some { parent; env; _ } -> Some (env.modules, parent)) *)

(*   let signature (scope : scope) *)
(*       ((qualifiers, unqualified_name) : Ast.Expr.func_label) = *)
(*     match *)
(*       lookup_mod_sig scope *)
(*         (List.append qualifiers [ unqualified_name ]) *)
(*         comptime_select *)
(*     with *)
(*     | `Ok { content = Module m; loc } -> *)
(*         Logger.error unqualified_name.loc "Found module instead of signature"; *)
(*         Logger.error loc "Module defined here"; *)
(*         `UnexpectedModule (Location.add_loc m loc) *)
(*     | `Ok { content = Signature found; loc } -> `Ok (Location.add_loc found loc) *)
(*     | (`Undefined _ | `UnexpectedSignature _) as other -> other *)

(*   let m0dule (scope : scope) *)
(*       ((qualifiers, unqualified_name) : Ast.Expr.func_label) = *)
(*     match *)
(*       lookup_mod_sig scope *)
(*         (List.append qualifiers [ unqualified_name ]) *)
(*         comptime_select *)
(*     with *)
(*     | `Ok { content = Module module'; loc } -> *)
(*         `Ok (Location.add_loc module' loc) *)
(*     | `Ok { content = Signature _; loc } -> *)
(*         Logger.error unqualified_name.loc "Found signature instead of module"; *)
(*         Logger.error loc "Signature defined here"; *)
(*         `UnexpectedSignature loc *)
(*     | `UnexpectedSignature _ as other -> other *)
(*     | `Undefined _ as other -> other *)

(*   let nested_signature (compiled_signatures : sig_scope) (scope : scope) *)
(*       ((qualifiers, unqualified_name) as names : Ast.Expr.func_label) = *)
(*     match *)
(*       lookup_mod_sig compiled_signatures *)
(*         (List.append qualifiers [ unqualified_name ]) *)
(*         signature_select *)
(*     with *)
(*     | `Ok _ as ok -> ok *)
(*     | `Undefined _ -> ( *)
(*         match signature scope names with *)
(*         | `Ok ok -> `Ok (Location.fmap (fun v -> PlainSignature v) ok) *)
(*         | (`Undefined _ | `UnexpectedModule _ | `UnexpectedSignature _) as error *)
(*           -> *)
(*             error) *)
(*     | `UnexpectedSignature _ as error -> error *)

(*   let predicate (scope : scope) *)
(*       ((qualifiers, ({ content = name; loc } as name_with_loc)) : *)
(*         Ast.Expr.func_label) (arity : int) = *)
(*     match lookup_mod_sig scope qualifiers comptime_select with *)
(*     | `Ok { content = Module comp_module; _ } -> ( *)
(*         let open Ast.Clause in *)
(*         match PredicateMap.find_opt { name; arity } comp_module.predicates with *)
(*         | None -> *)
(*             Logger.error loc "Undefined predicate"; *)
(*             `Undefined name_with_loc *)
(*         | Some predicate -> `Ok predicate) *)
(*     | `Ok { content = Signature _; loc = sig_loc } -> *)
(*         Logger.error loc "Qualifiers reference signature instead of module"; *)
(*         Logger.error sig_loc "Reference is here"; *)
(*         `UnexpectedSignature sig_loc *)
(*     | (`Undefined _ | `UnexpectedSignature _) as other -> other *)
(* end *)

(* Ascribing a signature means matching a module implementation with a given signature *)
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

let rec compile_multi_declaration
    (({ name; arity }, first_clause, remaining_clauses) :
      Ast.Clause.head
      * Ast.Clause.decl Location.with_location
      * Ast.Clause.decl Location.with_location list)
    ({ env; module_name; _ } as compiler : t) : t =
  let declaration =
    let args =
      if arity = 0 then []
      else List.map string_of_int @@ BatList.range 0 `To (arity - 1)
    in
    Beam.Builder.single_function_declaration name
      (List.map (fun v -> Beam.Builder.Pattern.Variable v) args)
    @@ compile_declaration_bodies module_name (first_clause :: remaining_clauses)
  in
  let export = Beam.Builder.Attribute.export [ (name, arity) ] in
  {
    compiler with
    output = FT.cons (FT.snoc compiler.output declaration) export;
    env =
      {
        env with
        predicates = PredicateMap.add { name; arity } () env.predicates;
      };
  }

and compile_clause (clause : Ast.Clause.t) (compiler : t) : t =
  (* TODO: handle location *)
  match clause.content with
  | Directive ({ loc; content = { name = _ :: _, _; _ } }, _) ->
      Logger.error loc "Directives cannot be qualified";
      exit 1
  | Directive ({ content = header; loc }, body) ->
      print_endline "Directives for some reason";
      Compiler.Directive.compile loc header body step compiler
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
and step : Ast.Clause.t list * t -> t = function
  | [], compiler ->
      compiler.persist compiler.filename
        (FT.append compiler.header compiler.output);
      compiler
  | clause :: remaining, compiler ->
      (* TODO: We should save the intermediary outputs, due to this being a step by module *)
      step (remaining, compile_clause clause compiler)
