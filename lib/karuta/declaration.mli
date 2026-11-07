open Beam
open Types

val call_with_fresh : string -> Builder.Expr.t -> Builder.Expr.t

val compile_single_body :
  state Shared.Compiler.t ->
  Ast.Module.decl Location.with_location ->
  Beam.Builder.Expr.t

val compile :
  Ast.head ->
  Ast.Module.decl Location.with_location
  * Ast.Module.decl Location.with_location FT.t ->
  state Shared.Compiler.t ->
  state Shared.Compiler.t
