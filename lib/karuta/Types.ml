type mods = { imports : Location.location BatMap.String.t }
type state = mods
type directives = Import of string Location.with_location

let initial_state () = { imports = BatMap.String.empty }
