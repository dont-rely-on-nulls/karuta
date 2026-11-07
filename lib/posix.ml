open Unix

let treat_pid pid =
  waitpid [] pid |> function
  | _pid, WEXITED 0 -> ()
  | _pid, WEXITED status ->
      Logger.simply_error
      @@ Printf.sprintf
           "Process terminated abnormally.\nExited with status code: %d" status;
      raise Exit
  | _pid, WSIGNALED signum ->
      Logger.simply_error
      @@ Printf.sprintf "Process terminated by a signal.\nSignal: %d" signum;
      raise Exit
  | _pid, WSTOPPED signum ->
      Logger.simply_error
      @@ Printf.sprintf "Process core dumped.\nSignal %d and core dumped" signum;
      raise Exit

let run_process args =
  let child_out, child_in = open_process_args args.(0) args in
  let pid = process_pid (child_out, child_in) in
  Out_channel.close child_in;
  let child_output = In_channel.input_all child_out in
  In_channel.close child_out;
  treat_pid pid;
  child_output

let process_status_message = function
  | Unix.WEXITED n -> "exited with code " ^ string_of_int n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      "interrupted by signal " ^ Sys.signal_to_string n
