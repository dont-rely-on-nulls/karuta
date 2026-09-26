let tests =
  Helpers.add_section_name "Parser"
    [
      ("Operators", Operators.tests);
      ("ifte", Ifte.tests);
      ("is_not", Is_not.tests);
      ("is", Is.tests);
    ]
