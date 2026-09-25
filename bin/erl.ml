open Unix
open Lib

let create_erl_process args = create_process "erl" args stdin stdout stderr

let spawn_compile_process program =
  [| "-noshell"; "-noinput"; "-eval"; program; "-s"; "erlang"; "halt" |]
  |> create_erl_process |> Posix.treat_pid

let compile prefix filepath forms =
  let open Beam.Serializer in
  let name = Lib.ModuleName.of_filepath filepath in
  let forms =
    "[" ^ (String.concat "," @@ List.map Attribute.to_string forms) ^ "]"
  in
  Logger.debug forms;
  let erlangProgram =
    "{ok, _, BeamByte} = compile:forms(" ^ forms ^ "), file:write_file(\""
    ^ prefix ^ "/" ^ name ^ ".beam\", BeamByte)"
  in
  Fun.const () @@ spawn_compile_process erlangProgram
