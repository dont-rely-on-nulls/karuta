open Shared.Compiler

let treat_error_cases
    ((qualifier, name) :
      string Location.with_location * string Location.with_location)
    (arity : int) : unit =
  if arity <> 0 then (
    Logger.error qualifier.loc "Sakura directives should be qualified atoms";
    exit 1)
  else if qualifier.content = "sakura" then (
    if BatSet.String.mem name.content Types.supported_directives then ()
    else Logger.error name.loc @@ "Undefined Sakura directive:" ^ name.content;
    exit 1)
  else (
    Logger.error qualifier.loc
      "Sakura directives should be qualified with 'sakura'";
    exit 1)

let rec swap a f =
  let v = Atomic.get a in
  if Atomic.compare_and_set a v (f v) then () else swap a f

let forbid_nested prefix directive_loc =
 (function
 | None -> ()
 | Some { Location.loc; _ } ->
     Logger.error directive_loc @@ prefix
     ^ " are not allowed inside of Sakura directives";
     Logger.error loc "Directive started here";
     exit 1)

let compile :
    (Types.state, Types.directives, Types.mods) runner ->
    Types.state t ->
    (Types.directives, Types.mods) Ast.Module.directive Location.with_location ->
    Types.state t =
 fun { step; initialize_nested }
     ({ env = { modules = _; _ } as _env; state; _ } as compiler :
       Types.state t) { content = directive; loc = directive_loc } ->
  let module Lookup = (val compiler.lookup) in
  match directive with
  | Module _ ->
      forbid_nested "Modules" directive_loc (Atomic.get state).current_directive;
      Shared.Directive.compile directive_loc directive step compiler
        initialize_nested
  | Signature _ ->
      forbid_nested "Signatures" directive_loc
        (Atomic.get state).current_directive;
      Logger.error directive_loc
        "TODO: Sakura has special treatment for signatures";
      exit 1
  | TargetSpecific (Ephemeral _declarations) ->
      Logger.error directive_loc
        "TODO: ephemeral directive is not yet implemented";
      exit 1
  | TargetSpecific (Constraint _declarations) ->
      Logger.error directive_loc
        "TODO: constraint directive is not yet implemented";
      exit 1
  | TargetSpecific (Persisted _declarations) ->
      Logger.error directive_loc
        "TODO: persisted directive is not yet implemented";
      exit 1
