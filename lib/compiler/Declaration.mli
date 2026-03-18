open Beam

val call_with_fresh : string -> Builder.Expr.t -> Builder.Expr.t

val compile_multi :
  Ast.Clause.head
  * Ast.Clause.decl Location.with_location
  * Ast.Clause.decl Location.with_location list ->
  Types.t ->
  Types.t
