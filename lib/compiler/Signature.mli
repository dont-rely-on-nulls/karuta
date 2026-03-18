val compile :
  Location.location ->
  Ast.Clause.t list ->
  Types.t ->
  Types.compiled_signature Location.with_location

val ascribe_to_module :
  Types.signature Location.with_location ->
  Types.compiled_module Location.with_location ->
  Types.compiled_module Location.with_location
