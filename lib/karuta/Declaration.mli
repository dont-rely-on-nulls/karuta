open Beam
open Types

val call_with_fresh : string -> Builder.Expr.t -> Builder.Expr.t

val compile_multi :
  Ast.head
  * Ast.Clause.decl Location.with_location
  * Ast.Clause.decl Location.with_location FT.t ->
  state Shared.Compiler.t ->
  state Shared.Compiler.t
