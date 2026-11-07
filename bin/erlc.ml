open Unix
open Lib

let create_erlc_process args = create_process "erlc" args stdin stdout stderr
let spawn_compile_process args = create_erlc_process args |> Posix.treat_pid

let dispatch_erlc prefix (abstr_files : string array) : unit =
  abstr_files
  |> Array.append [| "erlc"; "-o"; prefix |]
  |> spawn_compile_process |> Fun.const ()

module Persist = struct
  module Internal = Dict.Make (struct
    type key = string
    type value = unit

    let empty_value = ()
    let key_value_flush _ _ = ()
  end)

  let in_file = Internal.with_key
  let add () = Internal.update @@ Fun.const ()

  let compile prefix =
    Internal.flush_one_shot (fun file_unit_map ->
        file_unit_map |> BatMap.keys |> BatArray.of_enum |> dispatch_erlc prefix)
end

let create_file prefix filepath forms =
  let open Beam.Serializer in
  let name = Lib.Module_name.of_filepath filepath in
  let forms_str =
    (String.concat ".\n" @@ List.map Attribute.to_string forms) ^ ".\n"
  in
  Logger.simply_debug forms_str;
  let temp_file = prefix ^ "/" ^ name ^ ".abstr" in
  Logger.simply_debug @@ "Persisting " ^ name ^ " in " ^ temp_file;
  Out_channel.with_open_bin temp_file @@ fun oc ->
  output_string oc forms_str;
  temp_file

let compile_test prefix filepath forms =
  let open Beam.Serializer in
  let temp_file = create_file prefix filepath forms in
  Fun.const () @@ spawn_compile_process [| "erlc"; "-o"; prefix; temp_file |]
