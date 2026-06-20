open Types
open Shared.Compiler

let compile :
    (Types.state, Types.directives, Types.mods) runner ->
    Types.state t ->
    (Types.directives, Types.mods) Ast.Module.directive Location.with_location ->
    Types.state t =
 fun { step; initialize_nested } compiler
     { content = directive; loc = directive_loc } ->
  match directive with
  | Ast.Module.Module _ | Ast.Module.Signature _ ->
      Shared.Directive.compile directive_loc directive step compiler
        initialize_nested
  | Ast.Module.TargetSpecific (Import import_name) -> (
      let module Lookup = (val compiler.lookup) in
      let module_name' = (FT.empty, import_name) in
      let comptime_value = Lookup.ancestors_of_compiler compiler in
      match Lookup.m0dule comptime_value module_name' with
      | `Ok { loc; _ } | `UnexpectedSignature loc ->
          Logger.error import_name.loc "Import was already defined";
          Logger.error loc
            "There's already a module or signatures with the same name. \
             Modules and signatures share their scope.";
          exit 1
      | `Undefined _ ->
          let imports =
            BatMap.String.add import_name.content import_name.loc
              compiler.state.imports
          in
          { compiler with state = { imports } })
