open Cmdliner

type t = {
  files : string list;
  run : string option;
  log_level : Lib.Logger.Level.t;
}

let file_terms =
  let info =
    Arg.info [] ~doc:"Mandatory source files." ~docv:"FILE(.krt|.skr)"
  in
  Arg.non_empty (Arg.pos_all Arg.string [] info)

let run_term =
  let info =
    Arg.info [ "r"; "run" ]
      ~doc:
        "Optional function name argument to be called when running the program \
         after compilation."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let doc = "Compile Karuta and Sakura source files"
let man = [ `S Manpage.s_description; `P doc ]
let term combine = Term.(const combine $ file_terms $ run_term $ Log.term)

let cmd combine =
  let info = Cmdliner.Cmd.info "compile" ~doc ~man in
  Cmdliner.Cmd.v info (term combine)
