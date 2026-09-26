let add_section_name : string -> (string * 'a) list -> (string * 'a) list =
 fun section_name ->
  List.map (fun (name, test_suite) -> (section_name ^ " - " ^ name, test_suite))
