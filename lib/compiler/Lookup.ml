open Types

type 'a nested_env = 'a env BatLazyList.t
type scope = comptime nested_env
type sig_scope = signature nested_env

type 'a choice =
  [ `NestedLookup of 'a env | `UnexpectedSignature of Location.location ]

type 'a selector = 'a Location.with_location -> 'a choice

let (comptime_select : comptime selector) = function
  | { content = Module { modules; _ }; _ } -> `NestedLookup modules
  | { content = Signature _; loc = sig_loc } -> `UnexpectedSignature sig_loc

let (signature_select : signature selector) = function
  | { content = ModuleSignature { modules; _ }; _ } -> `NestedLookup modules
  | { content = PlainSignature _ | Abstract _; loc = sig_loc } ->
      `UnexpectedSignature sig_loc

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
        | `UnexpectedSignature sig_loc as unexpected ->
            Logger.error qualifier.loc
              "Qualifiers reference signature instead of module";
            Logger.error sig_loc "Reference is here";
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

let empty_signature : sig_scope = BatLazyList.nil

let sig_env_to_sig_scope (env : signature env) : sig_scope =
  BatLazyList.of_list [ env ]

let sig_cons (scope : sig_scope) (env : Types.sig_env) : sig_scope =
  BatLazyList.cons env scope

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
  | `Ok { content = Module module'; loc } -> `Ok (Location.add_loc module' loc)
  | `Ok { content = Signature _; loc } ->
      Logger.error unqualified_name.loc "Found signature instead of module";
      Logger.error loc "Signature defined here";
      `UnexpectedSignature loc
  | `UnexpectedSignature _ as other -> other
  | `Undefined _ as other -> other

let nested_signature (compiled_signatures : sig_scope) (scope : scope)
    ((qualifiers, unqualified_name) as names : Ast.Expr.func_label) =
  match
    lookup_mod_sig compiled_signatures
      (List.append qualifiers [ unqualified_name ])
      signature_select
  with
  | `Ok _ as ok -> ok
  | `Undefined _ -> (
      match signature scope names with
      | `Ok ok -> `Ok (Location.fmap (fun v -> PlainSignature v) ok)
      | (`Undefined _ | `UnexpectedModule _ | `UnexpectedSignature _) as error
        ->
          error)
  | `UnexpectedSignature _ as error -> error

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
  | `Ok { content = Signature _; loc = sig_loc } ->
      Logger.error loc "Qualifiers reference signature instead of module";
      Logger.error sig_loc "Reference is here";
      `UnexpectedSignature sig_loc
  | (`Undefined _ | `UnexpectedSignature _) as other -> other
