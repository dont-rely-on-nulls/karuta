let tests : Outcome.t BatMap.String.t =
  BatMap.String.of_list
    [
      ( "inline",
        {
          Outcome.filepaths = [ "module_system/inline.krt" ];
          root_module = "inline";
          limit = None;
          expected = [ [ ("Out", "1025") ] ];
        } );
      ( "nested",
        {
          Outcome.filepaths = [ "module_system/nested.krt" ];
          root_module = "nested";
          limit = None;
          expected = [ [ ("N", "413") ] ];
        } );
    ]
