open Unix

let treat_pid pid =
  waitpid [] pid |> function
  | _pid, WEXITED 0 -> Ok ()
  | _pid, (WEXITED status as err) ->
      Logger.simply_error
      @@ Printf.sprintf
           "Process terminated abnormally.\nExited with status code: %d" status;
      Error err
  | _pid, (WSIGNALED signum as err) ->
      Logger.simply_error
      @@ Printf.sprintf "Process terminated by a signal.\nSignal: %d" signum;
      Error err
  | _pid, (WSTOPPED signum as err) ->
      Logger.simply_error
      @@ Printf.sprintf "Process core dumped.\nSignal %d and core dumped" signum;
      Error err

let run_process args =
  let child_out, child_in = open_process_args args.(0) args in
  let pid = process_pid (child_out, child_in) in
  Out_channel.close child_in;
  let child_output = In_channel.input_all child_out in
  In_channel.close child_out;
  Error.(treat_pid pid ||> fun () -> Error.ok child_output)
