let () =
  let example_tests (target : Lib.Shared.Compiler.Options.target) prefix =
    Examples.tests
    |> List.map (fun (c, m) -> (prefix ^ c, BatMap.String.to_list m))
    |> List.map (Integration.make_test_suite (Unix.getcwd ()) target)
  in
  Alcotest.run "Karuta"
  @@ List.flatten
       [ example_tests Beam ""; example_tests Wasm "Wasm - "; Unit.tests ]
