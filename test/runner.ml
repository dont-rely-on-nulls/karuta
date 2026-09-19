type t = {
  filepaths : string list;
  root_module : string;
  expected : string;
  limit : int option;
}

let examples : t BatMap.String.t =
  BatMap.String.of_list
    [
      ( "plus",
        {
          filepaths = [ "arithmetic/plus.krt" ];
          root_module = "plus";
          limit = Some 2;
          expected =
            "---------\nLHS = 0\nRHS = 6\n---------\nLHS = -1\nRHS = 7\n";
        } );
      ( "minus",
        {
          filepaths = [ "arithmetic/minus.krt" ];
          root_module = "minus";
          limit = Some 2;
          expected =
            "---------\nLHS = 0\nRHS = 0\n---------\nLHS = -1\nRHS = -1\n";
        } );
    ]

let make_test current_directory
    (name, { expected; limit; root_module; filepaths }) =
  (try Unix.mkdir name 0o766 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  (try Unix.link "../runtime/karuta.beam" (name ^ "/karuta.beam")
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  ( name,
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
    | Ok () ->
        [
          ( Alcotest.test_case name `Quick @@ fun () ->
            Unix.chdir test_directory;
            match
              Lib.Posix.run_process
                (match limit with
                | None -> [| filename |]
                | Some n -> [| filename; string_of_int n |])
            with
            | Error e ->
                Alcotest.fail
                  ("Could not run example: " ^ name ^ " ("
                 ^ process_status_message e ^ ")")
            | Ok output ->
                Alcotest.(check string) ("Ran " ^ name) expected output );
        ] )

let () =
  Alcotest.run "Examples"
    (BatMap.String.to_seq examples
    |> BatSeq.map (make_test (Unix.getcwd ()))
    |> BatList.of_seq)
