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

let compile filepath _forms =
  let open Filename in
  let name = remove_extension @@ basename filepath in
  (* TODO: Serialize the list of forms *)
  let forms =
    "[{attribute,1,file,{\"" ^ name ^ ".krt\",1}}, {attribute,1,module," ^ name
    ^ "},\n\
      \     {attribute,3,export,[{hello,0}]},\n\
      \     {function,5,hello,0,\n\
      \               [{clause,5,[],[],\n\
      \                        [{call,6,\n\
      \                               {remote,6,{atom,6,io},{atom,6,format}},\n\
      \                               [{string,6,\"Hello World!\n\
       \"}]}]}]},\n\
      \     {eof,8}]"
  in
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
