open Cmdliner

type t = { file : string; function_name : string; should_repl : bool }

let file_term =
  let info =
    Arg.info [] ~doc:"Mandatory compiled BEAM source file (module name)."
      ~docv:"FILE.beam"
  in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let function_name_term =
  let info =
    Arg.info [] ~doc:"Mandatory function name within the compiled BEAM module."
  in
  Arg.required (Arg.pos 1 (Arg.some Arg.string) None info)

let should_repl_term =
  let info =
    Arg.info [ "r"; "repl" ]
      ~doc:
        "Optional flag to join ERL's repl with the compiled BEAM source file \
         loaded."
  in
  Arg.value (Arg.flag info)

let doc = "Run a compiled BEAM source file"
let man = [ `S Manpage.s_description; `P "Run a compiled BEAM source file." ]

let term combine =
  Term.(const combine $ file_term $ function_name_term $ should_repl_term)

let cmd combine =
  let info = Cmdliner.Cmd.info "run" ~doc ~man in
  Cmdliner.Cmd.v info (term combine)
