open Cmd
open Lib

let run : cmd -> unit = function
  | Compile { file } -> (
      match Executor.run file with
      | None -> failwith @@ "Could not execute file: " ^ file
      | Some computer ->
          print_endline @@ Crawler.StandardOut.query_string computer)

let () = exit @@ parse_command_line_and_run run
