include Shared.Lookup
include Types

type t = state Shared.Compiler.t

(* TODO: Update this later *)
let nested_env (compiler : t) _ = compiler.env
