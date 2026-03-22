open Cmdliner

let conv =
  Arg.enum
    [
      ("debug", Lib.Logger.Level.Debug);
      ("unreachable", Lib.Logger.Level.Unreachable);
      ("info", Lib.Logger.Level.Info);
      ("warning", Lib.Logger.Level.Warning);
      ("error", Lib.Logger.Level.Error);
    ]

let term =
  let info =
    Arg.info [ "log-level" ]
      ~doc:
        "Minimum log level to display. One of: debug, info, warning, error, \
         unreachable. Levels lower than the chosen value are suppressed."
      ~docv:"LEVEL"
  in
  Arg.value (Arg.opt conv Lib.Logger.Level.Debug info)
