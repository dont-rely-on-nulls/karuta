open Cmdliner

type cmd = Compile of Cmds.Compile.t | Run of Cmds.Run.t

let compile runner =
  let combine file run = Compile { file; run } |> runner in
  Cmds.Compile.cmd combine

let run runner =
  let combine file function_name should_repl =
    Run { file; function_name; should_repl } |> runner
  in
  Cmds.Run.cmd combine

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

let subcommands runner = [ compile runner; run runner; help ]

let parse_command_line_and_run (runner : cmd -> unit) =
  runner |> subcommands |> Cmd.group root_info |> Cmd.eval
