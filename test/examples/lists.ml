let tests : Outcome.t BatMap.String.t =
  BatMap.String.of_list
    [
      ( "main",
        {
          Outcome.filepaths = [ "lists/main.krt" ];
          root_module = "main";
          limit = Some 2;
          expected =
            [
              [
                ("AddOut", "[4, 6]");
                ("AppendOut", "[1, 2, 3, 4]");
                ("MainOut", "[]");
                ("RevOut", "[4, 3, 2, 1]");
              ];
              [
                ("AddOut", "[4, 6]");
                ("AppendOut", "[1, 2, 3, 4]");
                ("MainOut", "[[1000, 999]]");
                ("RevOut", "[4, 3, 2, 1]");
              ];
            ];
        } );
      ( "sort",
        {
          Outcome.filepaths = [ "lists/sort.krt" ];
          root_module = "sort";
          limit = None;
          expected = [ [ ("Sorted", "[0, 3, 11, 37, 1000]") ] ];
        } );
    ]
