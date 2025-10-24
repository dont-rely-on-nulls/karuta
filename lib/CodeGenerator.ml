module VariableMap = BatMap.Make (String)

type var_frequency_map = int VariableMap.t

module FT = BatFingerTree
module S = BatSet

type register_set = RegisterAllocator.register S.t

open Machine

type t = {
  p_register : int;
  terms : RegisterAllocator.term_queue;
  seen_registers : RegisterAllocator.register S.t;
  in_query : bool;
}

let initialize (begin_addr : int) : t =
  {
    p_register = begin_addr;
    terms = FT.empty;
    seen_registers = S.empty;
    in_query = false;
  }

let reset
    (({ p_register; _ }, allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  (initialize p_register, allocators, store)

let put_allocator (allocator : RegisterAllocator.t)
    ((generator, store) : t * Cell.t Store.t) :
    t * RegisterAllocator.t * Cell.t Store.t =
  (generator, allocator, store)

let cell_register : RegisterAllocator.register -> Cell.register = function
  | Temporary value -> Cell.X value
  | Permanent value -> Cell.Y value

and add_instruction (instruction : Cell.instruction)
    ((({ p_register; _ } as generator), store) : t * Cell.t Store.t) :
    t * Cell.t Store.t =
  (* TODO Add compiler builtins for better dev experience *)
  let real_instruction =
    match instruction with
    | Cell.Call ("debug", 0) -> Cell.Builtin Cell.Debug
    | Cell.Call ("int", 1) -> Cell.Builtin Cell.IsInteger
    | Cell.Call ("plus", 3) -> Cell.Builtin Cell.PlusInteger
    | Cell.Call ("neg", 2) -> Cell.Builtin Cell.NegateInteger
    | Cell.Call ("multiply", 3) -> Cell.Builtin Cell.MultiplyInteger
    | Cell.Call ("div-mod", 4) -> Cell.Builtin Cell.DivModInteger
    | Cell.Call ("leq", 2) -> Cell.Builtin Cell.LessThanOrEqualInteger
    | inst -> inst
  in
  let store =
    Store.code_put (Cell.Instruction real_instruction) p_register store
  in
  ({ generator with p_register = p_register + 1 }, store)

let add_constant constFn constant (generator, allocator, store) =
  (generator, store)
  |> add_instruction @@ constFn constant
  |> put_allocator allocator

module Argument = struct
  let rec emit_argument_and_queue_nested_if_not_in_query
      (variable : Cell.register -> Cell.instruction)
      (value : Cell.register -> Cell.instruction)
      (const : Cell.constant -> Cell.instruction)
      (( ({ terms; seen_registers; in_query; _ } as generator),
         ({ registers; _ } as allocator),
         store ) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.expr) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let raw_register = lazy (find elem registers) in
    let register = lazy (cell_register (Lazy.force raw_register)) in
    let new_seen_and_inst =
      lazy
        (let raw_register = Lazy.force raw_register in
         let register = Lazy.force register in
         match S.find_opt raw_register seen_registers with
         | None -> (S.add raw_register seen_registers, variable register)
         | Some _ -> (seen_registers, value register))
    in
    match elem with
    | Integer int ->
        add_constant const (Cell.Integer int) (generator, allocator, store)
    | Functor { namef; arity; _ } when arity = 0 ->
        add_constant const (Cell.Atom namef) (generator, allocator, store)
    | Variable { namev } ->
        let seen_registers, instruction = Lazy.force new_seen_and_inst in
        ({ generator with seen_registers }, store)
        |> add_instruction instruction
        |> (if in_query then
              add_instruction (Cell.QueryVariable (Lazy.force register, namev))
            else Fun.id)
        |> put_allocator allocator
    | Functor _ as f ->
        let seen_registers, instruction = Lazy.force new_seen_and_inst in
        let generator, store = add_instruction instruction (generator, store) in
        ( {
            generator with
            terms = (if in_query then terms else FT.cons terms f);
            seen_registers;
          },
          allocator,
          store )

  and emit_single_queued_argument
      (( ({ seen_registers; in_query; _ } as generator),
         ({ registers; _ } as allocator),
         store ) :
        t * RegisterAllocator.t * Cell.t Store.t) (elem : Ast.expr) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let raw_register = lazy (find elem registers) in
    let register = lazy (cell_register @@ Lazy.force raw_register) in
    let variable, value, func, list, const =
      if in_query then
        ( (fun v -> Cell.SetVariable v),
          (fun v -> Cell.SetValue v),
          (fun v -> Cell.PutStructure v),
          (fun (_, v) -> Cell.PutList v),
          fun v -> Cell.SetConstant v )
      else
        ( (fun v -> Cell.UnifyVariable v),
          (fun v -> Cell.UnifyValue v),
          (fun v -> Cell.GetStructure v),
          (fun (_, v) -> Cell.GetList v),
          fun v -> Cell.UnifyConstant v )
    in
    match elem with
    | Integer int ->
        (if in_query then Fun.id else add_constant const (Cell.Integer int))
          (generator, allocator, store)
    | Functor { namef; arity; _ } when arity = 0 ->
        (if in_query then Fun.id else add_constant const (Cell.Atom namef))
          (generator, allocator, store)
    | Variable _ ->
        if in_query then (generator, allocator, store)
        else
          let raw_register = Lazy.force raw_register in
          let register = Lazy.force register in
          let seen_registers, instruction =
            match S.find_opt raw_register seen_registers with
            | None -> (S.add raw_register seen_registers, variable register)
            | Some _ -> (seen_registers, value register)
          in
          ({ generator with seen_registers }, store)
          |> add_instruction instruction
          |> put_allocator allocator
    | Functor { namef; arity; elements } ->
        let raw_register = Lazy.force raw_register in
        let register = Lazy.force register in
        let instruction =
          (if namef = "" && arity = 2 then list else func)
            ((namef, arity), register)
        in
        let generator =
          { generator with seen_registers = S.add raw_register seen_registers }
        in
        let generator, allocator, store =
          if in_query then
            List.fold_left emit_single_queued_argument
              (generator, allocator, store)
              elements
          else (generator, allocator, store)
        in
        let generator, store = add_instruction instruction (generator, store) in
        List.fold_left
          (emit_argument_and_queue_nested_if_not_in_query variable value const)
          (generator, allocator, store)
          elements

  and emit_queued_arguments
      ((({ terms; _ } as generator), allocator, store) :
        t * RegisterAllocator.t * Cell.t Store.t) :
      t * RegisterAllocator.t * Cell.t Store.t =
    match FT.front terms with
    | None -> (generator, allocator, store)
    | Some (rest, elem) ->
        elem
        |> emit_single_queued_argument
             ({ generator with terms = rest }, allocator, store)
        |> emit_queued_arguments

  and emit
      (( ({ seen_registers; in_query; _ } as generator),
         ({ registers; _ } as allocator),
         store ) :
        t * RegisterAllocator.t * Cell.t Store.t) (index : int)
      (elem : Ast.expr) : t * RegisterAllocator.t * Cell.t Store.t =
    let open RegisterAllocator.RegisterMap in
    let raw_register = lazy (find elem registers) in
    let register = lazy (cell_register (Lazy.force raw_register)) in
    let arg_register = Cell.X index in
    let variable, value, func, list, const =
      if in_query then
        ( (fun v -> Cell.PutVariable v),
          (fun v -> Cell.PutValue v),
          (fun v -> Cell.PutStructure v),
          (fun (_, v) -> Cell.PutList v),
          fun v -> Cell.PutConstant v )
      else
        ( (fun v -> Cell.GetVariable v),
          (fun v -> Cell.GetValue v),
          (fun v -> Cell.GetStructure v),
          (fun (_, v) -> Cell.GetList v),
          fun v -> Cell.GetConstant v )
    in
    match elem with
    | Integer int ->
        add_constant const
          (Cell.Integer int, arg_register)
          (generator, allocator, store)
    | Functor { namef; arity; _ } when arity = 0 ->
        add_constant const
          (Cell.Atom namef, arg_register)
          (generator, allocator, store)
    | Variable { namev } ->
        let raw_register = Lazy.force raw_register in
        let register = Lazy.force register in
        let seen_registers, instruction =
          match S.find_opt raw_register seen_registers with
          | None ->
              ( S.add raw_register seen_registers,
                variable (register, arg_register) )
          | Some _ -> (seen_registers, value (register, arg_register))
        in
        ({ generator with seen_registers }, store)
        |> add_instruction instruction
        |> (if in_query then
              add_instruction (Cell.QueryVariable (arg_register, namev))
            else Fun.id)
        |> put_allocator allocator
    | Functor { namef; arity; elements } ->
        let raw_register = Lazy.force raw_register in
        let instruction =
          (if namef = "" && arity = 2 then list else func)
            ((namef, arity), arg_register)
        in
        let generator =
          { generator with seen_registers = S.add raw_register seen_registers }
        in
        if in_query then
          let generator, allocator, store =
            List.fold_left emit_single_queued_argument
              (generator, allocator, store)
              elements
          in
          let generator, store =
            add_instruction instruction (generator, store)
          in
          let generator, allocator, store =
            List.fold_left
              (emit_argument_and_queue_nested_if_not_in_query
                 (fun v -> Cell.SetVariable v)
                 (fun v -> Cell.SetValue v)
                 (fun v -> Cell.SetConstant v))
              (generator, allocator, store)
              elements
          in
          (generator, allocator, store)
        else
          let generator, store =
            add_instruction instruction (generator, store)
          in
          List.fold_left
            (emit_argument_and_queue_nested_if_not_in_query
               (fun v -> Cell.UnifyVariable v)
               (fun v -> Cell.UnifyValue v)
               (fun v -> Cell.UnifyConstant v))
            (generator, allocator, store)
            elements
          |> emit_queued_arguments

  let owl = Fun.compose Fun.compose Fun.compose

  let emit_functor_argument =
    owl emit_queued_arguments
    @@ emit_argument_and_queue_nested_if_not_in_query
         (fun v -> Cell.SetVariable v)
         (fun v -> Cell.SetValue v)
         (fun v -> Cell.SetConstant v)
end

let rec emit_head ({ elements; _ } : Ast.func)
    ((generator, ({ y_register; _ } as allocator), store) :
      t * RegisterAllocator.t * Cell.t Store.t) =
  let instruction = Cell.Allocate y_register in
  let generator, store = add_instruction instruction (generator, store) in
  Seq.fold_lefti Argument.emit
    (generator, allocator, store)
    (List.to_seq elements)

and emit_body (elements : Ast.func list) (generator, allocator, store) :
    t * RegisterAllocator.t * Cell.t Store.t =
  let emit_clause (generator, allocator, store)
      ({ namef; elements; arity } : Ast.func) :
      t * RegisterAllocator.t * Cell.t Store.t =
    let emit_argument
        (( ( ({ seen_registers; _ } as generator),
             ({ registers; _ } as allocator),
             store ),
           counter ) :
          (t * RegisterAllocator.t * Cell.t Store.t) * int)
        (individual_element : Ast.expr) :
        (t * RegisterAllocator.t * Cell.t Store.t) * int =
      (* TODO: Fix this with ppx for constructors as functions *)
      let constFn constant = Cell.PutConstant constant in
      match individual_element with
      | Integer int ->
          ( add_constant constFn
              (Cell.Integer int, Cell.X counter)
              (generator, allocator, store),
            counter + 1 )
      | Functor { namef; arity; _ } when arity = 0 ->
          ( add_constant constFn
              (Cell.Atom namef, Cell.X counter)
              (generator, allocator, store),
            counter + 1 )
      | Variable _ as var ->
          let open RegisterAllocator.RegisterMap in
          let raw_register = find var registers in
          let left_register = cell_register raw_register in
          let seen_registers, instruction =
            match S.find_opt raw_register seen_registers with
            | None ->
                ( S.add raw_register seen_registers,
                  Cell.PutVariable (left_register, Cell.X counter) )
            | Some _ ->
                (seen_registers, Cell.PutValue (left_register, Cell.X counter))
          in
          let generator, store =
            add_instruction instruction
              ({ generator with seen_registers }, store)
          in
          ((generator, allocator, store), counter + 1)
      | Functor func as f ->
          let open RegisterAllocator.RegisterMap in
          let (generator, allocator, store), _ =
            (generate_functor (generator, allocator, store) func, counter)
          in
          let left_register = cell_register @@ find f registers in
          let instruction = Cell.PutValue (left_register, Cell.X counter) in
          let generator, store =
            add_instruction instruction (generator, store)
          in
          ((generator, allocator, store), counter + 1)
    in
    let (generator, allocator, store), _ =
      List.fold_left emit_argument ((generator, allocator, store), 0) elements
    in
    let instruction = Cell.Call (namef, arity) in
    (generator, store) |> add_instruction instruction |> put_allocator allocator
  in
  let generator, allocator, store =
    List.fold_left emit_clause (generator, allocator, store) elements
  in
  let previous_p_register = generator.p_register - 1 in
  match Store.code_get store previous_p_register with
  | Cell.Instruction (Cell.Call lastCallFunctor) ->
      (generator, store)
      |> add_instruction (Cell.Execute lastCallFunctor)
      |> (fun (generator, store) ->
           ( generator,
             Store.code_put (Cell.Instruction Cell.Deallocate)
               previous_p_register store ))
      |> put_allocator allocator
  | Cell.Instruction (Cell.Builtin _) ->
      (generator, store)
      |> add_instruction Cell.Deallocate
      |> add_instruction Cell.Proceed
      |> put_allocator allocator
  | _ -> failwith "something went very wrong when generating a tail call"

and generate_functor
    ( ({ seen_registers; _ } as generator),
      ({ registers; _ } as allocator),
      store ) ({ elements; arity; namef } as func : Ast.func) =
  let open RegisterAllocator.RegisterMap in
  let raw_register = find (Ast.Functor func) registers in
  let register = cell_register raw_register in
  let instruction =
    if namef = "" && arity = 2 then Cell.PutList register
    else if arity = 0 then Cell.PutConstant (Cell.Atom namef, register)
    else Cell.PutStructure ((namef, arity), register)
  in
  let seen_registers = S.add raw_register seen_registers in
  let generator, store =
    add_instruction instruction ({ generator with seen_registers }, store)
  in
  List.fold_left Argument.emit_functor_argument
    (generator, allocator, store)
    elements

and swap_allocators (allocators : RegisterAllocator.t list)
    ((generator, _, store) : t * RegisterAllocator.t * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  (generator, allocators, store)

and generate_single_declaration (decl : Ast.decl)
    ((generator, allocator :: allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  match decl with
  | { head = { elements; _ }; body = [] } ->
      let gas =
        Seq.fold_lefti Argument.emit
          (generator, allocator, store)
          (List.to_seq elements)
      in
      let generator, allocator, store = Argument.emit_queued_arguments gas in
      (generator, store)
      |> add_instruction Cell.Proceed
      |> put_allocator allocator |> swap_allocators allocators |> reset
  | { head; body } ->
      (generator, allocator, store)
      |> emit_head head |> emit_body body |> swap_allocators allocators |> reset

and generate_declaration_and_patch (inst : int -> Cell.instruction)
    (decl : Ast.decl)
    ((generator, allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  let address_to_patch = generator.p_register in
  (generator, store) |> add_instruction Cell.Halt (* Placeholder *)
  |> fun (generator, store) ->
  (generator, allocators, store) |> generate_single_declaration decl
  |> fun (({ p_register; _ } as generator), allocators, store) ->
  ( generator,
    allocators,
    Store.code_put (Cell.Instruction (inst p_register)) address_to_patch store
  )

and generate
    ((generator, allocators, store) :
      t * RegisterAllocator.t list * Cell.t Store.t) (value : Ast.clause) :
    t * RegisterAllocator.t list * Cell.t Store.t =
  match value with
  | QueryConjunction { namef; elements; arity } ->
      let (allocator :: allocators) = allocators in
      let generator = { generator with in_query = true } in
      let generator, allocator, store =
        Seq.fold_lefti Argument.emit
          (generator, allocator, store)
          (List.to_seq elements)
      in
      let instruction = Cell.Call (namef, arity) in
      (generator, store)
      |> add_instruction instruction
      (* |> add_instruction Cell.Debug *)
      |> add_instruction Cell.Halt
      |> (fun (generator, store) ->
           ({ generator with seen_registers = S.empty }, store))
      |> put_allocator allocator
      |> swap_allocators allocators (* TODO: deal with multiple queries *)
  | MultiDeclaration (decl, []) ->
      let generator = { generator with in_query = false } in
      (generator, allocators, store) |> generate_single_declaration decl
  | MultiDeclaration (first, decls) ->
      let generator = { generator with in_query = false } in
      let rec split_last (l : 'a list) : 'a list * 'a =
        match l with
        | [] -> failwith "impossible"
        | [ final ] -> ([], final)
        | first :: rest ->
            let left, final = split_last rest in
            (first :: left, final)
      in
      let middle_decls, last_decl = split_last decls in
      (generator, allocators, store)
      |> generate_declaration_and_patch (fun n -> Cell.TryMeElse n) first
      |> fun acc ->
      List.fold_left
        (Fun.flip
        @@ generate_declaration_and_patch (fun n -> Cell.RetryMeElse n))
        acc middle_decls
      |> generate_declaration_and_patch (Fun.const Cell.TrustMe) last_decl
