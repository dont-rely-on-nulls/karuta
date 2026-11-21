open Cmdliner

type t = { path : string; page_size : int; log_level : Lib.Logger.Level.t }

let path_term =
  let info =
    Arg.info [ "p"; "path" ]
      ~doc:"Directory holding the runtime's compiled .beam files."
  in Arg.value @@ Arg.opt Arg.string "runtime" info

let page_size_term =
  let info =
    Arg.info [ "page-size" ] ~docv:"ROWS"
      ~doc:"How many solutions a query shows at once; ';' shows the next page."
  in Arg.value @@ Arg.opt Arg.int 10 info

let doc = "Start an interactive Karuta session"
let man = [ `S Manpage.s_description; `P doc ]

let cmd combine =
  let term = Term.(const combine $ path_term $ page_size_term $ Log.term)
  in Cmdliner.Cmd.v (Cmdliner.Cmd.info "repl" ~doc ~man) term

