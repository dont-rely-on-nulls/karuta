open Builder

let eq (lhs : Expr.t) (rhs : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "eq") [ lhs; rhs ]

let call_with_fresh (f : Expr.func) : Expr.t =
  call_with_module (atom "karuta") (atom "call_with_fresh") [ Expr.Fun f ]

let disj (goals : Expr.t list) : Expr.t =
  call_with_module (atom "karuta") (atom "disj") goals

let conj (goals : Expr.t list) : Expr.t =
  call_with_module (atom "karuta") (atom "conj") goals

let take_all (stream : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "take_all") [ stream ]

let start (goal : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "start") [ goal ]
