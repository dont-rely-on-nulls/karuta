open Cmdliner

let conv =
  let open Lib.Logger.Level in
  Arg.enum (List.map (fun l -> (String.lowercase_ascii (show l), l)) all)

let term =
  let info =
    Arg.info [ "log-level" ]
      ~doc:
        "Minimum log level to display. One of: debug, info, warning, error, \
         unreachable. Levels lower than the chosen value are suppressed."
      ~docv:"LEVEL"
  in
  Arg.value (Arg.opt conv Lib.Logger.Level.Debug info)
