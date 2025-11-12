open Cmdliner

type cmd = Compile of Cmds.Compile.t

let compile run =
  let combine file = Compile { file } |> run in
  Cmds.Compile.cmd combine

let root_doc = "Welcome to Karuta!"

let root_man =
  [
    `S Manpage.s_description;
    `P "A relational programming language for both applications and databases!";
  ]

let root_term = Term.ret (Term.const (`Help (`Pager, None)))
let root_info = Cmd.info "karuta" ~doc:root_doc ~man:root_man

let help =
  let info = Cmd.info "help" in
  Cmd.v info root_term

let subcommands run = [ compile run; help ]

let parse_command_line_and_run (run : cmd -> unit) =
  run |> subcommands |> Cmd.group root_info |> Cmd.eval
