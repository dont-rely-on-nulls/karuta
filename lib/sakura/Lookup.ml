include Shared.Lookup
include Types

type t = state Shared.Compiler.t

let nested_env (compiler : t) = compiler.env
