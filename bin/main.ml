open Cmd
open Lib

let show_clauses (clauses : Ast.clause list) : string =
  let folder acc clause = acc ^ Ast.show_clause clause in
  List.fold_left folder "" clauses

let run : cmd -> unit = function
  | Compile { file } -> (
      match Executor.run file with
      | [] -> failwith @@ "Could not run file: " ^ file
      | clauses -> print_endline @@ show_clauses clauses)

let () = exit @@ parse_command_line_and_run run
