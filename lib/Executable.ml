open Shared.Compiler

type payload = {
  persist : Persist.raw;
  query : Ast.head;
  sakura : Options.sakura option;
  filename : string;
  root_module : string;
}

let emit { persist; query = { name; arity }; sakura; filename; root_module } =
  let shebang = "#!/usr/bin/env lfescript\n" in
  let config_map =
    match sakura with
    | None -> "(map)"
    | Some { address; port; _ } ->
        Printf.sprintf "(map 'db_port %d 'db_address \"%s\" 'db_root '|%s|)"
          port address root_module
  in
  let query =
    Printf.sprintf "(call '%s '|%s| %s)" root_module name
      (String.concat " " @@ List.init (arity + 1) (Fun.const "'||"))
  in
  let body =
    Printf.sprintf
      "  (let ((take-fn\n\
      \          (case args\n\
      \            ((list) (fun karuta:take-all 1))\n\
      \            ((list 'inf) (fun karuta:take-all 1))\n\
      \            ((list n) (lambda (results)\n\
      \                        (karuta:take (list_to_integer n) results))))))\n\
      \    (karuta:pretty-print (funcall take-fn (karuta:run-lazy %s %s))))"
      config_map query
  in
  let main = "(defun main (args)\n" ^ body ^ ")\n" in
  persist filename @@ shebang ^ main
