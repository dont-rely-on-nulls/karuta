include Types
include Shared.Lookup

type t = state Shared.Compiler.t

let rec print_module externals =
  BatMap.String.print BatInnerIO.write_string
    (fun out { Location.content; _ } ->
      match content with
      | Shared.Compiler.Module { modules; predicates; _ } ->
          print_module modules;
          Shared.Compiler.PredicateMap.print ~first:"|" ~last:"|"
            (fun out h -> BatInnerIO.write_string out (Ast.show_head h))
            (fun out _ -> BatInnerIO.write_string out "()")
            out predicates
      | Signature _ -> BatInnerIO.write_string out "<Sig>")
    BatInnerIO.stderr externals
