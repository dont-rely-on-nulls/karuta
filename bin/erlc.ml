open Unix
open Lib

let create_erlc_process args = create_process "erlc" args stdin stdout stderr
let spawn_compile_process args = create_erlc_process args |> Posix.treat_pid

let create_tmp_file name forms_str =
  let temp_dir = Filename.get_temp_dir_name () in
  let temp_file = Filename.concat temp_dir (name ^ ".abstr") in
  Out_channel.with_open_bin temp_file @@ fun oc ->
  output_string oc forms_str;
  temp_file

let compile prefix filepath forms =
  let open Beam.Serializer in
  let name = Lib.Module_name.of_filepath filepath in
  let forms_str =
    (String.concat ".\n" @@ List.map Attribute.to_string forms) ^ ".\n"
  in
  Logger.debug forms_str;
  let temp_file = create_tmp_file name forms_str in
  Logger.debug @@ "Persisting " ^ name ^ " in " ^ temp_file;
  Fun.const () @@ spawn_compile_process [| "erlc"; "-o"; prefix; temp_file |]
