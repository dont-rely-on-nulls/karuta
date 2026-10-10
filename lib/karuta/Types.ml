type mods = { imports : Location.region BatMap.String.t }
type state = mods
type directives = |

let init_state = Fun.id
let merge_state = Fun.const
