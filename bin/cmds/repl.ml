open Cmdliner

type t = { files : string list }

let file_term =
  let info =
    Arg.info [] ~doc:"List of optional Karuta source files." ~docv:"[FILE.krt]"
  in
  Arg.value (Arg.pos_all Arg.string [] info)

let doc = "Start the interactive REPL with optional list of Karuta source files"
let man = [ `S Manpage.s_description; `P "Start the interactive REPL." ]
let term combine = Term.(const combine $ file_term)

let cmd combine =
  let info = Cmdliner.Cmd.info "repl" ~doc ~man in
  Cmdliner.Cmd.v info (term combine)
