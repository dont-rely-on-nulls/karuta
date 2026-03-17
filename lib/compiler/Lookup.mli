type sig_scope
type scope

val empty_signature : sig_scope
val sig_env_to_sig_scope : Types.sig_env -> sig_scope
val sig_cons : sig_scope -> Types.sig_env -> sig_scope
val ancestors_of_compiler : Types.t -> scope

val signature :
  scope ->
  Ast.Expr.func_label ->
  [> `Ok of Types.compiled_signature Location.with_location
  | `Undefined of string Location.with_location
  | `UnexpectedModule of Types.compiled_module Location.with_location
  | `UnexpectedSignature of Location.location ]

val m0dule :
  scope ->
  Ast.Expr.func_label ->
  [> `Ok of Types.compiled_module Location.with_location
  | `Undefined of string Location.with_location
  | `UnexpectedSignature of Location.location ]

val nested_signature :
  sig_scope ->
  scope ->
  Ast.Expr.func_label ->
  [> `Ok of Types.signature Location.with_location
  | `Undefined of string Location.with_location
  | `UnexpectedModule of Types.compiled_module Location.with_location
  | `UnexpectedSignature of Location.location ]

val predicate :
  scope ->
  Ast.Expr.func_label ->
  int ->
  [> `Ok of unit
  | `Undefined of string Location.with_location
  | `UnexpectedSignature of Location.location ]
