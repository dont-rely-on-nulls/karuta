open Cmd
open Lib
open Error

(* let show_clauses (clauses : Ast.clause list) : string = *)
(*   let folder acc clause = acc ^ Ast.show_clause clause in *)
(*   List.fold_left folder "" clauses *)

let check_extensions (ext : BatSet.String.t) (files : string list) : unit =
  let ext_string () =
    let out = BatIO.output_string () in
    BatSet.String.print ~first:"Valid extensions: '" ~sep:"', '" ~last:"'"
      BatIO.nwrite out ext;
    BatIO.close_out out
  in
  let check_single_extension (file : string) : unit =
    if not @@ BatSet.String.mem (Filename.extension file) ext then (
      Logger.simply_error @@ display (InvalidExtension file);
      Logger.simply_info @@ ext_string ();
      exit 1)
  in
  List.iter check_single_extension files

let run : cmd -> unit = function
  (* TODO: Introduce multiple files to compile subcommand *)
  | Compile { files; run; log_level } ->
      Logger.Level.set_min_level log_level;
      check_extensions (BatSet.String.of_list [ ".krt"; ".skr"; ".pl" ]) files;
      let* _ =
        Executor.compile
          (fun name forms ->
            Erl.compile "runtime" name @@ BatFingerTree.to_list forms)
          files
      in
      (* TODO: fix call to run *)
      Option.iter
        (fun function_name -> Erl.run (List.hd files) function_name false)
        run
  | Run { file; function_name; should_repl; log_level } ->
      Logger.Level.set_min_level log_level;
      check_extensions (BatSet.String.of_list [ ".beam" ]) [ file ];
      Erl.run file function_name should_repl

let () = exit @@ parse_command_line_and_run run
