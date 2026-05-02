include Compiler.Lookup

let ancestors_of_compiler (compiler : t) : scope =
  let open BatLazyList in
  unfold (Some compiler) (function
    | None -> None
    | Some { parent; env; _ } -> Some (env.modules, parent))
