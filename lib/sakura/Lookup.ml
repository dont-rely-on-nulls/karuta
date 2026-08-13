include Shared.Lookup
include Types

type t = state Shared.Compiler.t

let comptime_of_compiler (compiler : t) =
  Location.add_loc (Shared.Compiler.Module compiler.env) Location.dummy
