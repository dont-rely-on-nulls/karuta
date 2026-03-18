val compile :
  Location.location ->
  Ast.Expr.func ->
  Ast.Clause.t list list ->
  (Ast.Clause.t list * Types.t -> Types.t) ->
  Types.t ->
  Types.t
