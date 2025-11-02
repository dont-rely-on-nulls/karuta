module Option = struct
  let ( let+ ) = Option.bind
end

let bimap f g (a1, a2) = (f a1, g a2)

let update_store (computer : Machine.t) (store : Machine.Cell.t Machine.Store.t)
    : Machine.t =
  { computer with store }

let run file =
  let open Option in
  let+ content =
    In_channel.with_open_text file (fun fc ->
        try Some (In_channel.input_all fc) with End_of_file -> None)
  in
  match Parse.parse content with
  | [] ->
      print_endline "File could not be parsed.";
      None
  | decls_queries ->
      let compiler, computer =
        Machine.initialize () |> fun initialComputer ->
        Compiler.compile
          ( Preprocessor.group_clauses decls_queries,
            Compiler.initialize (),
            initialComputer.store )
        |> bimap Fun.id (update_store initialComputer)
      in
      let+ entry_point = compiler.entry_point in
      let computer =
        Evaluator.eval compiler.functor_table
          { computer with p_register = entry_point.p_register }
      in
      Some computer
