type mods = { imports : Location.location BatMap.String.t }
type state = mods
type directives = Import of string Location.with_location

let init_state = Fun.id

let merge_state ({ imports = mods_imports } : mods) { imports = state_imports }
    : state =
  {
    imports =
      BatMap.String.union
        (fun _ import _ -> Some import)
        mods_imports state_imports;
  }
