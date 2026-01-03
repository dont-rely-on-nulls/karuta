open Cmd
open Lib
open Error

(* let show_clauses (clauses : Ast.clause list) : string = *)
(*   let folder acc clause = acc ^ Ast.show_clause clause in *)
(*   List.fold_left folder "" clauses *)

let check_extension (ext : string) (files : string list) : unit =
  let check_single_extension (file : string) : unit =
    if ext <> Filename.extension file then (
      Logger.simply_error @@ display (InvalidExtension file);
      Logger.simply_info @@ "Valid extensions: '" ^ ext ^ "'";
      exit 1)
  in
  List.iter check_single_extension files

let run : cmd -> unit = function
  (* TODO: Introduce multiple files to compile subcommand *)
  | Compile { file; run } ->
      check_extension ".krt" [ file ];
      let* forms = Executor.compile file in
      Erl.compile file @@ BatFingerTree.to_list forms;
      Option.iter (fun function_name -> Erl.run file function_name false) run
  | Run { file; function_name; should_repl } ->
      check_extension ".beam" [ file ];
      Erl.run file function_name should_repl

let () = exit @@ parse_command_line_and_run run
