open Builder

let call_with_fresh (f : Expr.func) : Expr.t =
  call_with_module (atom "kanren") (atom "call-with-fresh") [ Expr.Fun f ]

let disj (goals : Expr.t list) : Expr.t =
  call_with_module (atom "kanren") (atom "disj") [ list_expr goals ]

let conj (goals : Expr.t list) : Expr.t =
  call_with_module (atom "kanren") (atom "conj") [ list_expr goals ]

let query_variable var_name goal : Expr.t =
  call_with_module (atom "kanren") (atom "query-variable")
    [ var var_name; atom var_name; goal ]
