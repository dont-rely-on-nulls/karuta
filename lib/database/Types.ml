type inner_state = { schemas : BatSet.String.t }
type state = inner_state ref

let initial_state () = ref { schemas = BatSet.String.empty }
