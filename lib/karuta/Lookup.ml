include Types
include Shared.Lookup

type t = state Shared.Compiler.t

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
