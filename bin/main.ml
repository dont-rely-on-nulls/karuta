open Cmd
open Lib

let show_clauses (clauses : Ast.clause list) : string =
  let folder acc clause = acc ^ Ast.show_clause clause in
  List.fold_left folder "" clauses

open Error

let run : cmd -> unit = function
  | Compile { file } -> (
      match Executor.run file with
      | Error err -> failwith @@ display err
      | Ok clauses -> 
        print_endline @@ show_clauses clauses;
        let compiler_state = Compiler.initialize file in
        let forms = Compiler.compile (clauses, compiler_state) in
        print_endline "\nErlang Abstract Format:";
        print_endline @@ Compiler.serialize_to_erlang_abstract forms)

let () = exit @@ parse_command_line_and_run run
