open Types
open Shared.Compiler

let compile :
    (Types.state, Types.directives) runner ->
    Types.state t ->
    Types.directives Ast.Module.directive Location.with_location ->
    Types.state t =
 fun runner compiler { content = directive; loc = directive_loc } ->
  Shared.Directive.compile directive_loc directive compiler runner
