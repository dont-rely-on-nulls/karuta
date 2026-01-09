open Builder

let eq (lhs : Expr.t) (rhs : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "eq") [ lhs; rhs ]

let nat (n : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "nat") [ n ]

let call_with_fresh (f : Expr.func) : Expr.t =
  call_with_module (atom "karuta") (atom "call_with_fresh") [ Expr.Fun f ]

let disj (goals : Expr.t list) : Expr.t =
  call_with_module (atom "karuta") (atom "disj") [ list_expr goals ]

let conj (goals : Expr.t list) : Expr.t =
  call_with_module (atom "karuta") (atom "conj") [ list_expr goals ]

let take_all (stream : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "take_all") [ stream ]

let start (goal : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "start") [ goal ]

let query_variable var_name goal : Expr.t =
  call_with_module (atom "karuta") (atom "query_variable")
    [ var var_name; atom var_name; goal ]

let run_lazy (goal : Expr.t) : Expr.t =
  call_with_module (atom "karuta") (atom "run_lazy") [ goal ]
