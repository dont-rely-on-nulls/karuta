open Unix
open Lib

let create_erl_process args = create_process "erl" args stdin stdout stderr

let treat_pid pid =
  waitpid [] pid |> function
  | _pid, WEXITED 0 -> Ok ()
  | (_pid, WEXITED status) as err ->
      Logger.simply_error
      @@ Printf.sprintf
           "Process terminated abnormally.\nExited with status code: %d" status;
      Error err
  | (_pid, WSIGNALED signum) as err ->
      Logger.simply_error
      @@ Printf.sprintf "Process terminated by a signal.\nSignal: %d" signum;
      Error err
  | (_pid, WSTOPPED signum) as err ->
      Logger.simply_error
      @@ Printf.sprintf "Process core dumped.\nSignal %d and core dumped" signum;
      Error err

let spawn_compile_process program =
  [| "-noshell"; "-noinput"; "-eval"; program; "-s"; "erlang"; "halt" |]
  |> create_erl_process |> treat_pid

let compile filepath forms =
  let open Filename in
  let open Beam.Serializer in
  let name = remove_extension @@ basename filepath in
  let forms =
    "[" ^ (String.concat "," @@ List.map Attribute.to_string forms) ^ "]"
  in
  print_endline forms;
  let erlangProgram =
    "{ok, _, BeamByte} = compile:forms(" ^ forms ^ "), file:write_file(\""
    ^ name ^ ".beam\", BeamByte)"
  in
  Fun.const () @@ spawn_compile_process erlangProgram

let spawn_run_process filepath function_name should_repl =
  let open Filename in
  let name = remove_extension @@ basename filepath in
  let args =
    if should_repl then [| "-noinput"; "-s"; name; function_name |]
    else
      [|
        "-noshell";
        "-noinput";
        "-s";
        name;
        function_name;
        "-s";
        "erlang";
        "halt";
      |]
  in
  args |> create_erl_process |> treat_pid

let run filepath function_name should_repl =
  Fun.const () @@ spawn_run_process filepath function_name should_repl
