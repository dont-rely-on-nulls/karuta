open Cmdliner

type t = { file : string; run : string option }

let file_term =
  let info =
    Arg.info [] ~doc:"Mandatory Karuta source file." ~docv:"FILE.krt"
  in
  Arg.required (Arg.pos 0 (Arg.some Arg.file) None info)

let run_term =
  let info =
    Arg.info [ "r"; "run" ]
      ~doc:
        "Optional function name argumento to be called when running the \
         program after compilation."
  in
  Arg.value (Arg.opt (Arg.some Arg.string) None info)

let doc = "Compile a Karuta source file"
let man = [ `S Manpage.s_description; `P "Compile a Karuta source file." ]
let term combine = Term.(const combine $ file_term $ run_term)

let cmd combine =
  let info = Cmdliner.Cmd.info "compile" ~doc ~man in
  Cmdliner.Cmd.v info (term combine)
