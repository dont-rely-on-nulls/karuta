let tests : Outcome.t BatMap.String.t =
  BatMap.String.of_list
    [
      ( "dependencies",
        {
          Outcome.filepaths =
            [
              "dependencies/main.krt";
              "dependencies/a.krt";
              "dependencies/b.krt";
              "dependencies/c.krt";
              "dependencies/d.krt";
              "dependencies/e.krt";
            ];
          root_module = "main";
          limit = None;
          expected = [ [ ("Color", "green"); ("N", "2") ] ];
        } );
    ]
