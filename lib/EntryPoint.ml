open Compiler

type payload = {
  persist : Persist.raw;
  query : Ast.head;
  sakura : Options.sakura option;
  filename : string;
  root_module : string;
}

let emit { persist; query = { name; arity }; sakura; filename; root_module } =
  let shebang = "#!/usr/bin/env escript\n" in
  let config_map =
    match sakura with
    | None -> "#{}"
    | Some { address; port; _ } ->
        Printf.sprintf "#{db_port => %d, db_address => \"%s\", db_root => '%s'}"
          port address root_module
  in
  let query =
    Printf.sprintf "%s:'%s'(%s)" root_module name
      (String.concat ", " @@ List.init (arity + 1) (Fun.const "''"))
  in
  let body =
    Printf.sprintf
      "io:format(\"~p~n\", [karuta:'take-all'(karuta:'run-lazy'(%s, %s))])"
      config_map query
  in
  let main = "main(_) ->\n  " ^ body ^ ".\n" in
  persist filename @@ shebang ^ main
