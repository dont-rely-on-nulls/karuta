open Outcome

let ( |.| ) = Fun.compose

let print_string_set =
  BatIO.to_string
    (BatSet.String.print ~first:"" ~last:"" ~sep:" " BatString.print)

let pp_value ppf v = Format.pp_print_string ppf (print_string_set v)
let batset_string = Alcotest.testable pp_value BatSet.String.equal

let format_expected : Outcome.expected -> BatSet.String.t =
  let concat acc (key, value) = acc ^ key ^ " = " ^ value ^ "\n" in
  BatSet.String.of_list |.| List.map (List.fold_left concat "")

let divisor = "---------\n"

let split_results : string -> BatSet.String.t =
  BatSet.String.of_list |.| List.tl |.| BatString.split_on_string ~by:divisor

let make_test current_directory
    (name, { expected; limit; root_module; filepaths }) =
  (try Unix.mkdir name 0o766 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.link "../runtime/karuta.beam" (name ^ "/karuta.beam")
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let test_directory = current_directory ^ "/" ^ name in
  let prefix file = current_directory ^ "/../examples/" ^ file in
  let filename = test_directory ^ "/play" in
  let open Lib.Error in
  match
    Lib.Executor.compile
      {
        artifact =
          Lib.Shared.Compiler.Options.Executable { filename; root_module };
        sakura = None;
      }
      {
        beam =
          (fun name forms ->
            Bin.Erl.compile test_directory name @@ BatFingerTree.to_list forms);
        executable = Lib.Executable.create_file;
      }
      (List.map prefix filepaths)
  with
  | Error e ->
      Alcotest.fail
        ("Could not compile example: " ^ name ^ " (" ^ display e ^ ")")
  | Ok () -> (
      Alcotest.test_case name `Quick @@ fun () ->
      Unix.chdir test_directory;
      match
        Lib.Posix.run_process
          (match limit with
          | None -> [| filename |]
          | Some n -> [| filename; string_of_int n |])
      with
      | Error e ->
          Alcotest.fail
            ("Could not run example: " ^ name ^ " (" ^ process_status_message e
           ^ ")")
      | Ok output ->
          output |> split_results
          |> Alcotest.(check batset_string)
               ("Ran " ^ name) (format_expected expected))

let make_test_suite current_directory (category, tests) =
  (category, List.map (make_test current_directory) tests)
