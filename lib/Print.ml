open Machine

let rec inspect (store : Cell.t Store.t) (register : Cell.t) : string =
  match register with
  | Constant const -> ( match const with Integer i -> string_of_int i)
  | Structure address -> (
      match Store.get store address with
      | Functor (name, arity) ->
          let open Batteries in
          let folder acc elem =
            let inner = inspect store @@ Store.get store (address + elem + 1) in
            if acc = "" then inner else acc ^ "," ^ inner
          in
          let middle = List.fold_left folder "" (List.of_enum (0 --^ arity)) in
          if arity = 0 then name else name ^ "[" ^ middle ^ "]"
      | _ -> failwith "Nonsense via structure")
  | Reference _ -> (
      let next = Evaluator.deref_cell register store in
      match next with
      | Reference _ -> failwith "TODO: take care of free variables"
      | _ -> inspect store next)
  | Empty | Functor _ | ArgCount _ | Instruction _ | Address _ ->
      failwith "unreachable inspect"

let query_args ({ store; query_variables; _ } : Machine.t) : string =
  let folder acc (variable, cell) =
    acc ^ variable ^ " = " ^ inspect store cell ^ "\n"
  in
  BatSeq.fold_left folder "" (BatMap.to_seq query_variables)
