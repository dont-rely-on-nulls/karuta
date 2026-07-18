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

let ancestors_of_compiler (compiler : t) : Shared.Compiler.scope =
  let open struct
    type ancestors_of_compiler =
      | Compiler of t
      | Imports of Shared.Compiler.comptime Shared.Compiler.env
      | End
  end in
  let open BatLazyList in
  unfold (Compiler compiler) (function
    | End -> None
    | Imports imports -> Some (imports, End)
    | Compiler { parent; env; state; externals; _ } ->
        let imports = state.imports in
        Some
          ( env.modules,
            Option.fold
              ~none:
                (Imports
                   (BatMap.String.filter
                      (fun k _ -> BatMap.String.mem k imports)
                      externals))
              ~some:(fun c -> Compiler c)
              parent ))
