let () =
  Examples.tests
  |> List.map (fun (c, m) -> (c, BatMap.String.to_list m))
  |> List.map (Helpers.make_test_suite (Unix.getcwd ()))
  |> Alcotest.run "Examples"
