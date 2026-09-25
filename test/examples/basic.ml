let tests : Outcome.t BatMap.String.t =
  BatMap.String.of_list
    [
      ( "conjunction",
        {
          Outcome.filepaths = [ "basic/conjunction.krt" ];
          root_module = "conjunction";
          limit = None;
          expected = [ [ ("First", "4"); ("Out", "6") ] ];
        } );
      ( "functor",
        {
          Outcome.filepaths = [ "basic/functor.krt" ];
          root_module = "functor";
          limit = None;
          expected = [ [ ("What", "abc[def]") ] ];
        } );
    ]
