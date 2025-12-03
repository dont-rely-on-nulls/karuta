type entry_point = { p_register : int }
type functor_name = string * int [@@deriving ord]

module FunctorMap = BatMap.Make (struct
  type t = functor_name [@@deriving ord]
end)
[@@warning "-32"]

type functor_map = int FunctorMap.t

let show_functor_table (functors : functor_map) : string =
  let open FunctorMap in
  BatSeq.fold_left
    (fun acc ((label, arity), address) ->
      acc ^ label ^ "/" ^ string_of_int arity ^ ":" ^ string_of_int address
      ^ "\n")
    "" (to_seq functors)

type t = {
  entry_point : entry_point option;
  p_register : int;
  functor_table : functor_map;
}

let initialize () : t =
  { entry_point = None; p_register = 0; functor_table = FunctorMap.empty }

and compile : Ast.clause list * t -> unit = function _, _ -> ()
