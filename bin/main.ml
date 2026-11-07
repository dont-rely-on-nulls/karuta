open Bin.Cmd
open Lib
module Erlc = Bin.Erlc

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
      Logger.simply_error @@ "Input file '" ^ file ^ "' has invalid extension";
      Logger.simply_info @@ ext_string ();
      raise Exit)
  in
  List.iter check_single_extension files

let run : cmd -> unit = function
  | Compile { files; artifact; output_path; log_level } ->
      (* TODO: Make sakura module name as an available CLI option with db being the default *)
      Logger.with_min_level log_level @@ fun () ->
      check_extensions (BatSet.String.of_list [ ".krt"; ".skr"; ".pl" ]) files;
      let open Shared.Compiler in
      let options : Options.t =
        Options.initialize
          ~sakura:
            (Some (Options.initialize_sakura ~address:"localhost" ~port:3435 ()))
          ~artifact ()
      in
      Executor.compile options
        {
          beam =
            (fun name forms ->
              let file =
                Erlc.create_file output_path name @@ BatFingerTree.to_list forms
              in
              Erlc.Persist.in_file file @@ fun () -> Erlc.Persist.add ());
          executable =
            (fun name -> Executable.create_file (output_path ^ "/" ^ name));
        }
        files;
      Erlc.Persist.compile output_path

let () =
  Eio_main.run @@ fun _ ->
  at_exit Logger.flush;
  exit @@ parse_command_line_and_run run
