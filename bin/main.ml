open Cmd
open Lib
open Unix
open Error

let spawn_process program =
  let args =
    [| "-noshell"; "-noinput"; "-eval"; program; "-s"; "erlang"; "halt" |]
  in
  let pid = create_process "erl" args stdin stdout stderr in
  waitpid [] pid |> function
  | _pid, _a -> print_endline "[INFO] Compilation finished"

let beamify filepath _forms =
  let open Filename in
  let name = remove_extension @@ basename filepath in
  (* TODO: Serialize the list of forms *)
  let forms =
    "[{attribute,1,file,{\"" ^ name ^ ".erl\",1}},{attribute,1,module," ^ name
    ^ "},{attribute,2,export,[{hello_world,0}]},{function,4,hello_world,0,[{clause,4,[],[],[{integer,4,2}]}]},{eof,5}]"
  in
  let erlangProgram =
    "{ok, _, BeamByte} = compile:forms(" ^ forms ^ "), file:write_file(\""
    ^ name ^ ".beam\", BeamByte)"
  in
  spawn_process erlangProgram

let show_clauses (clauses : Ast.clause list) : string =
  let folder acc clause = acc ^ Ast.show_clause clause in
  List.fold_left folder "" clauses

let run : cmd -> unit = function
  (* TODO: Introduce multiple files to compile subcommand *)
  | Compile { file } -> (
      match Executor.run file with
      | Error err -> failwith @@ display err
      | Ok clauses ->
         (* TODO: Remove this when executor's run is finished *)
         print_endline @@ show_clauses clauses;
         beamify file []) 

let () = exit @@ parse_command_line_and_run run
