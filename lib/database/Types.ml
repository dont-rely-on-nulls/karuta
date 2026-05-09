type inner_state = { schemas : BatSet.String.t }
type state = inner_state Atomic.t

let initial_state () = Atomic.make { schemas = BatSet.String.empty }
