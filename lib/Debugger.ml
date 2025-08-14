type action =
  | ShowQueryVariables
  | ShowInternalRegisters
  | ShowXRegisters
  | ShowHeap
  | ShowStack
  | ShowCode
  | ShowFunctorTable
  (* TODO: Make this overwrite subsequent debug instructions until program ends *)
  | DisableDebug
  | ToggleTrace
  | Halt
  | Help
  | Step
[@@deriving enumerate]

type description = string
type shortcut = string

let action_info : action -> shortcut * description = function
  | ShowInternalRegisters -> ("r", "Display internal registers")
  | ShowXRegisters -> ("x", "Display temporary (X) registers")
  | ShowHeap -> ("m", "Display heap memory region")
  | ShowStack -> ("s", "Display stack memory region")
  | ShowCode -> ("c", "Display code memory region")
  | ShowFunctorTable -> ("f", "Display functor table")
  | ShowQueryVariables -> ("query", "Display query variables")
  | DisableDebug -> ("q", "Disable debug mode")
  | ToggleTrace -> ("t", "Toggle trace")
  | Halt -> ("halt", "Halt execution")
  | Help -> ("h", "Display list of all debug actions")
  | Step -> ("", "Proceed to next instruction")

let help_message : string =
  let format_empty_string shortcut =
    if shortcut = "" then "<EMPTY>" else shortcut
  in
  let format_action action =
    let shortcut, description = action_info action in
    format_empty_string shortcut ^ " : " ^ description
  in
  let folder acc elem = acc ^ "\n" ^ format_action elem in
  List.fold_left folder "" all_of_action ^ "\n\n"

let parse_action (cmd : shortcut) : action option =
  let shortcuts =
    List.map (fun action -> (action, fst @@ action_info action)) all_of_action
  in
  let match_action (_, shortcut) = shortcut = cmd in
  match List.find_opt match_action shortcuts with
  | Some (action, _) -> Some action
  | None -> None

let rec run (functor_table : Compiler.functor_map)
    (stepper : Compiler.functor_map -> Machine.t -> Machine.t * bool)
    (eval : Compiler.functor_map -> Machine.t -> Machine.t)
    (computer : Machine.t) : Machine.t =
  if computer.debug then (
    let action = read_line () in
    match parse_action action with
    | Some ShowQueryVariables ->
        print_string @@ Compiler.show_query_variables computer.query_variables;
        run functor_table stepper eval computer
    | Some ShowInternalRegisters ->
        print_endline @@ Machine.show_internal_registers computer;
        run functor_table stepper eval computer
    | Some ShowXRegisters ->
        print_endline @@ Machine.show_x_registers computer.x_registers;
        run functor_table stepper eval computer
    | Some ShowHeap ->
        print_endline
        @@ Machine.show_store computer.store Machine.Store.heap_start
             (Machine.Store.heap_start + Machine.Store.Layout.heap_size);
        run functor_table stepper eval computer
    | Some ShowStack ->
        print_endline
        @@ Machine.show_store computer.store Machine.Store.stack_start
             (Machine.Store.stack_start + Machine.Store.Layout.stack_size);
        run functor_table stepper eval computer
    | Some ShowCode ->
        print_endline
        @@ Machine.show_store computer.store 0 Machine.Store.Layout.code_size;
        run functor_table stepper eval computer
    | Some ShowFunctorTable ->
        print_string @@ Compiler.show_functor_table functor_table;
        run functor_table stepper eval computer
    | Some DisableDebug ->
        run functor_table stepper eval { computer with debug = false }
    | Some ToggleTrace ->
        print_string "Trace ";
        print_endline @@ if computer.trace then "off" else "on";
        run functor_table stepper eval
          { computer with trace = not computer.trace }
    | Some Halt -> computer
    | Some Help ->
        print_string help_message;
        run functor_table stepper eval computer
    | Some Step ->
        let computer, stop = stepper functor_table computer in
        if stop then computer else run functor_table stepper eval computer
    | None ->
        print_endline "Unknown debug action";
        run functor_table stepper eval computer)
  else eval functor_table computer
