let _ =
  (* TODO: Fix this CLI on Issue 11 *)
  if Array.length Sys.argv = 2 then
    let file = Sys.argv.(1) in
    match Lib.Executor.run file with
    | None -> failwith @@ "Could not execute file: " ^ file
    | Some computer -> print_endline @@ Lib.Crawler.query_string computer
  else failwith "Missing file to compile"
