let () =
  let example_tests =
    Examples.tests
    |> List.map (fun (c, m) -> (c, BatMap.String.to_list m))
    |> List.map (Integration.make_test_suite (Unix.getcwd ()))
  in
  Alcotest.run "Karuta" @@ List.flatten [ example_tests; Unit.tests ]
