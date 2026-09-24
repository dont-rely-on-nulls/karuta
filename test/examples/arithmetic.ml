let tests : Outcome.t BatMap.String.t =
  BatMap.String.of_list
    [
      ( "plus",
        {
          Outcome.filepaths = [ "arithmetic/plus.krt" ];
          root_module = "plus";
          limit = Some 2;
          expected =
            [ [ ("LHS", "0"); ("RHS", "6") ]; [ ("LHS", "-1"); ("RHS", "7") ] ];
        } );
      ( "minus",
        {
          Outcome.filepaths = [ "arithmetic/minus.krt" ];
          root_module = "minus";
          limit = Some 2;
          expected =
            [ [ ("LHS", "0"); ("RHS", "0") ]; [ ("LHS", "-1"); ("RHS", "-1") ] ];
        } );
      ( "multiply",
        {
          Outcome.filepaths = [ "arithmetic/multiply.krt" ];
          root_module = "multiply";
          limit = Some 2;
          expected =
            [
              [ ("LHS", "1"); ("Out", "12"); ("RHS", "12") ];
              [ ("LHS", "-1"); ("Out", "12"); ("RHS", "-12") ];
            ];
        } );
      ( "factorial",
        {
          Outcome.filepaths = [ "arithmetic/factorial.krt" ];
          root_module = "factorial";
          limit = None;
          expected = [ [ ("Out", "120") ] ];
        } );
      ( "triangular",
        {
          Outcome.filepaths = [ "arithmetic/triangular.krt" ];
          root_module = "triangular";
          limit = None;
          expected = [ [ ("Out", "15") ] ];
        } );
    ]
