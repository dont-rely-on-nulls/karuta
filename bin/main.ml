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
      (* TODO: Make sakura module name as an available CLI option with db being the default *)
      let open Compiler.Types in
      let options : Options.t =
        Options.initialize
          ~sakura:
            (Some (Options.initialize_sakura ~address:"localhost" ~port:3435 ()))
          ~artifact:(Executable { filename = "deal"; root_module = "plus" })
          ()
      in
      let prefix = "runtime" in
      let* _ =
        Executor.compile options
          {
            beam =
              (fun name forms ->
                Erl.compile prefix name @@ BatFingerTree.to_list forms);
            executable =
              (fun name body ->
                let full_name = prefix ^ "/" ^ name in
                Out_channel.with_open_text full_name (fun c ->
                    Out_channel.output_string c body);
                Unix.chmod full_name 0o755);
          }
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
