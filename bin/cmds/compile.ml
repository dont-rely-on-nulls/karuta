open Cmdliner

type t = {
  files : string list;
  artifact : Lib.Shared.Compiler.Options.artifact;
  log_level : Lib.Logger.Level.t;
}

let file_terms =
  let info =
    Arg.info [] ~doc:"Mandatory source files." ~docv:"FILE(.krt|.skr)"
  in
  Arg.non_empty (Arg.pos_all Arg.string [] info)

let artifact_term =
  let library_term =
    Arg.value @@ Arg.flag
    @@ Arg.info [ "lib" ] ~doc:"Compile a library; do not emit an entry point."
  in
  let root_term =
    let info =
      Arg.info [ "r"; "root" ]
        ~doc:"Name of the module whose query will be the program's entry point."
    in
    Arg.value @@ Arg.opt Arg.string "main" info
  in
  let executable_filename_term =
    let info = Arg.info [ "o"; "executable" ] ~doc:"Name of the executable." in
    Arg.value @@ Arg.opt Arg.string "play" info
  in
  let open Lib.Shared.Compiler.Options in
  Term.product library_term @@ Term.product root_term executable_filename_term
  |> Term.map (function
    | true, _ -> Library
    | false, (root_module, filename) -> Executable { root_module; filename })

let doc = "Compile Karuta and Sakura source files"
let man = [ `S Manpage.s_description; `P doc ]
let term combine = Term.(const combine $ file_terms $ artifact_term $ Log.term)

let cmd combine =
  let info = Cmdliner.Cmd.info "compile" ~doc ~man in
  Cmdliner.Cmd.v info (term combine)
