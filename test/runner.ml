type t = {
  name : string;
  filepaths : string list;
  root_module : string;
  expected : string;
  limit : int option;
}

let examples : t list =
  [
    {
      name = "plus";
      filepaths = [ "arithmetic/plus.krt" ];
      root_module = "plus";
      limit = Some 2;
      expected = "---------\nLHS = 0\nRHS = 6\n---------\nLHS = -1\nRHS = 7\n";
    };
  ]

let make_test { name; expected; limit; root_module; filepaths } =
  Unix.chdir "../runtime";
  let prefix file = "../examples/" ^ file in
  match
    Lib.Executor.run ~limit
      { filename = "./play"; root_module }
      {
        beam =
          (fun name forms ->
            Bin.Erl.compile "" name @@ BatFingerTree.to_list forms);
        executable = Lib.Executable.create_file;
      }
      (List.map prefix filepaths)
  with
  | Error _ -> (name, Alcotest.fail ("Could not run example: " ^ name))
  | Ok output ->
      ( name,
        [
          ( Alcotest.test_case name `Quick @@ fun () ->
            Alcotest.(check string) ("Ran " ^ name) expected output );
        ] )

let () = Alcotest.run "Examples" (List.map make_test examples)
