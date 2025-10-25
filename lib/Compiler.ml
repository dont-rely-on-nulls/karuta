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

let show_query_variables (variables : Machine.query_map) : string =
  BatSeq.fold_left
    (fun acc (name, cell) -> acc ^ name ^ " = " ^ Machine.Cell.show cell ^ "\n")
    "" (BatMap.to_seq variables)

type t = {
  entry_point : entry_point option;
  p_register : int;
  functor_table : functor_map;
}

let initialize () : t =
  { entry_point = None; p_register = 0; functor_table = FunctorMap.empty }

let rec allocate_registers (elem : Ast.clause) : RegisterAllocator.t list =
  match elem with
  | (MultiDeclaration _ | Query _) as form ->
      RegisterAllocator.allocate_toplevel form

and compile :
    Ast.clause list * t * Machine.Cell.t Machine.Store.t ->
    t * Machine.Cell.t Machine.Store.t = function
  | [], compiler, store -> (compiler, store)
  | d :: ds, ({ entry_point; p_register; functor_table } as compiler), store
    -> (
      match d with
      | MultiDeclaration ({ head; _ }, _) as form ->
          let open FunctorMap in
          ( allocate_registers form |> fun allocators ->
            CodeGenerator.generate
              (CodeGenerator.initialize p_register, allocators, store)
              form )
          |> fun (code_generator, _, store) ->
          compile
            ( ds,
              {
                compiler with
                p_register = code_generator.p_register;
                functor_table =
                  add (head.namef, head.arity) p_register functor_table;
              },
              store )
      | Query _ as form ->
          let entry_point =
            match entry_point with
            | None -> Some { p_register }
            | Some _ ->
                failwith
                  "Multiple queries are only supported using the comma syntax"
          in
          ( allocate_registers form |> fun allocator ->
            CodeGenerator.generate
              (CodeGenerator.initialize p_register, allocator, store)
              form )
          |> fun (code_generator, _, store) ->
          compile
            ( ds,
              {
                compiler with
                entry_point;
                p_register = code_generator.p_register;
              },
              store ))
