open Unix

let spawn_process program =
  let args =
    [| "-noshell"; "-noinput"; "-eval"; program; "-s"; "erlang"; "halt" |]
  in
  let pid = create_process "erl" args stdin stdout stderr in
  waitpid [] pid |> function
  | pid_, _a -> print_endline "[INFO] Compilation finished"

let compile name =
  let forms =
    "[{attribute,1,file,{\"" ^ name ^ ".erl\",1}},{attribute,1,module," ^ name
    ^ "},{attribute,2,export,[{hello_world,0}]},{function,4,hello_world,0,[{clause,4,[],[],[{integer,4,2}]}]},{eof,5}]"
  in
  let program =
    "{ok, _, BeamByte} = compile:forms(" ^ forms ^ "), file:write_file(\""
    ^ name ^ ".beam\", BeamByte)"
  in
  spawn_process program

let () = compile "hello"
