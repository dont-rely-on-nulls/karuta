open Types

let compile :
    Location.location ->
    (directives, mods) Ast.Module.directive ->
    ((directives, mods) Ast.Module.module_body * state Shared.Compiler.t ->
    state Shared.Compiler.t) ->
    state Shared.Compiler.t ->
    (state, mods) Shared.Compiler.initialize_nested ->
    state Shared.Compiler.t =
 fun directive_loc directive step compiler initialize_nested ->
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
