open Machine

(* TODO: Consider a functor out of this later for the most general cases *)
module type Crawler = sig
  val query_args : Machine.t -> Query.variable_map
  val query_string : Machine.t -> string
end

module StandardOut : Crawler = struct
  let rec inspect_list (store : Cell.t Store.t) (address : int) : string =
    let head = inspect store (Reference address) in
    let tail =
      match Evaluator.deref_cell (Reference (address + 1)) store with
      | Cell.Constant (Cell.Atom "") -> "]"
      | Cell.List next_address -> ", " ^ inspect_list store next_address
      | others -> " | " ^ inspect store others ^ "]"
    in
    head ^ tail

  and inspect (store : Cell.t Store.t) (register : Cell.t) : string =
    match register with
    | Constant const -> (
        match const with Integer i -> string_of_int i | Atom atom -> atom)
    | Cell.List address -> "[" ^ inspect_list store address
    | Structure address -> (
        match Store.get store address with
        | Functor (name, arity) ->
            let open Batteries in
            let folder acc elem =
              let inner =
                inspect store @@ Store.get store (address + elem + 1)
              in
              if acc = "" then inner else acc ^ "," ^ inner
            in
            let middle =
              List.fold_left folder "" (List.of_enum (0 --^ arity))
            in
            if arity = 0 then name else name ^ "[" ^ middle ^ "]"
        | _ -> failwith "Nonsense via structure")
    | Reference _ -> (
        let next = Evaluator.deref_cell register store in
        match next with
        | Reference _ -> failwith "TODO: take care of free variables"
        | _ -> inspect store next)
    | Empty | Functor _ | ArgCount _ | Instruction _ | Address _ ->
        failwith "unreachable inspect"

  let query_args ({ store; query_variables; _ } : Machine.t) :
      Query.variable_map =
    let mapper cell = inspect store cell in
    BatMap.map mapper query_variables

  let query_string (computer : Machine.t) : string =
    let folder variable cell acc = acc ^ variable ^ " = " ^ cell ^ "\n" in
    BatMap.foldi folder (query_args computer) ""
end

module Tabulation : Crawler = struct
  open Ascii_table

  let rec inspect_list (store : Cell.t Store.t) (address : int) : string list =
    let head = inspect store (Reference address) in
    let tail =
      match Evaluator.deref_cell (Reference (address + 1)) store with
      | Cell.Constant (Cell.Atom "") -> []
      | Cell.List next_address -> inspect_list store next_address
      | others -> inspect store others
    in
    head @ tail

  and inspect (store : Cell.t Store.t) (register : Cell.t) : string list =
    match register with
    | Constant const -> (
        match const with
        | Integer i -> [ string_of_int i ]
        | Atom atom -> [ atom ])
    | Cell.List address -> inspect_list store address
    | Structure address -> (
        match Store.get store address with
        | Functor (name, arity) ->
            let open Batteries in
            let folder (acc : string list) elem : string list =
              let inner =
                inspect store @@ Store.get store (address + elem + 1)
              in
              if acc = [] then inner else acc @ inner
            in
            let middle : string list =
              List.fold_left folder [] (List.of_enum (0 --^ arity))
            in
            if arity = 0 then [ name ] else name :: middle
        | _ -> failwith "Nonsense via structure")
    | Reference _ -> (
        let next = Evaluator.deref_cell register store in
        match next with
        | Reference _ -> failwith "TODO: take care of free variables"
        | _ -> inspect store next)
    | Empty | Functor _ | ArgCount _ | Instruction _ | Address _ ->
        failwith "unreachable inspect"

  let query_string ({ store; query_variables; _ } : Machine.t) : string =
    let folder (acc : (string, string list) BatMap.t) (variable, cell) :
        (string, string list) BatMap.t =
      BatMap.add variable (inspect store cell) acc
    in
    let vars =
      BatMap.to_seq
      @@ BatSeq.fold_left folder BatMap.empty (BatMap.to_seq query_variables)
    in
    let inner : string Column.t list = [ Column.create "" Fun.id ] in
    let columns : (string * string list) Column.t list =
      List.of_seq
      @@ Seq.map
           (fun (name, _) ->
             Column.create name (fun (_, (content : string list)) ->
                 Ascii_table.to_string_noattr ~bars:`Unicode inner content))
           vars
    in
    Ascii_table.to_string ~header_attr:[ `Magenta ] ~bars:`Unicode columns
    @@ List.of_seq vars

  let query_args : Machine.t -> Query.variable_map =
   fun _ -> failwith "query_args is not implemented for Tabulation"
end
