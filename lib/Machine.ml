module Cell = struct
  type register = X of int | Y of int

  and instruction =
    | GetStructure of ((string * int) * register)
    | PutStructure of ((string * int) * register)
    | PutVariable of (register * register)
    | GetVariable of (register * register)
    | SetVariable of register
    | SetValue of register
    | UnifyVariable of register
    | GetValue of (register * register)
    | PutValue of (register * register)
    | UnifyValue of register
    | Call of (Ast.tag * int)
    | Execute of (Ast.tag * int)
    | Proceed
    | Allocate of int
    | Deallocate
    | TryMeElse of int
    | RetryMeElse of int
    | TrustMe
    | Halt
    | QueryVariable of (register * string)
    | Debug
  [@@deriving show]

  and t =
    | Structure of int
    | Reference of int
    | Functor of string * int
    | Address of int
    | ArgCount of int
    | Instruction of instruction
    | Empty
  [@@deriving show]

  let address_from_cell (cell : t) : int =
    match cell with Address n -> n | _ -> failwith "of_address"
end

module Mode = struct
  type t = Read | Write
end

module IM = BatIMap

type query_map = (string, Cell.t) BatMap.t

module Store = Store.Make (struct
  let code_size = 1000
  let heap_size = 1000
  let stack_size = 1000
  let pdl_size = 1000
  let trail_size = 1000
end)

module IntMap = Map.Make (Int)

type t = {
  store : Cell.t Store.t;
  arg_count : int;
  x_registers : Cell.t IntMap.t;
  (* Cell.t IM.t; *)
  b_register : int;
  h_register : int;
  hb_register : int;
  s_register : int;
  p_register : int;
  cp_register : int;
  e_register : int;
  tr_register : int;
  mode : Mode.t;
  fail : bool;
  debug : bool;
  trace : bool;
  args : int option;
  query_variables : query_map;
}

let show_store (store : Cell.t Store.t) (start_index : int) (end_index : int) :
    string =
  match
    Option.map
      (Fun.compose fst
         (Store.fold_left
            (fun (s, n) elem ->
              ( (if elem = Cell.Empty then s
                 else s ^ string_of_int n ^ ": " ^ " " ^ Cell.show elem ^ "\n"),
                n + 1 ))
            ("", start_index)))
    @@ Store.window start_index end_index store
  with
  | None -> "Invalid bounds"
  | Some s -> s

let show_x_registers (registers : Cell.t IntMap.t) : string =
  let open IntMap in
  fold
    (fun key value acc ->
      acc ^ "\n" ^ string_of_int key ^ " = " ^ Cell.show value)
    registers ""
[@@warning "-32"]

let show_internal_registers (computer : t) : string =
  "P: "
  ^ string_of_int computer.p_register
  ^ "\n" ^ "CP: "
  ^ string_of_int computer.cp_register
  ^ "\n" ^ "E: "
  ^ string_of_int computer.e_register
  ^ "\n" ^ "B: "
  ^ string_of_int computer.b_register
  ^ "\n" ^ "H: "
  ^ string_of_int computer.h_register
  ^ "\n" ^ "HB: "
  ^ string_of_int computer.hb_register
  ^ "\n" ^ "S: "
  ^ string_of_int computer.s_register
  ^ "\n" ^ "TR: "
  ^ string_of_int computer.tr_register

let initialize () : t =
  {
    store = Store.initialize Store.empty Store.mem_size Cell.Empty;
    arg_count = 0;
    x_registers = IntMap.empty;
    p_register = 0;
    cp_register = 0;
    e_register = Store.stack_start - 1;
    b_register = Store.stack_start - 2;
    h_register = Store.heap_start;
    hb_register = Store.heap_start;
    s_register = Store.heap_start;
    tr_register = Store.trail_start;
    mode = Mode.Read;
    fail = false;
    debug = false;
    trace = false;
    args = None;
    query_variables = BatMap.empty;
  }
