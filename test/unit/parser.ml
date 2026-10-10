let tests =
  List.concat
    [
      Helpers.add_section_name "PEG"
        [
          ("Operators", Operators.tests);
          ("ifte", Ifte.tests);
          ("is_not", Is_not.tests);
          ("is", Is.tests);
          ("maybe", Maybe.tests);
          ("ignoring", Ignoring.tests);
          ("star", Star.tests);
          ("plus", Plus.tests);
          ("some", Some.tests);
        ];
      Helpers.add_section_name "Grammar"
        [
          ("horizontal_whitespace", Horizontal_whitespace.tests);
          ("line", Line.tests);
          ("whitespace", Whitespace.tests);
          ("atom", Atom.tests);
          ("variable", Variable.tests);
          ("quoted_atom", Quoted_atom.tests);
          ("integer", Integer.tests);
          ("func_label", Func_label.tests);
          ("expr", Expr.tests);
          ("query", Query.tests);
          ("declaration", Declaration.tests);
          ("parser_clause", Parser_clause.tests);
          ("parse", Parse.tests);
        ];
      Helpers.add_section_name "Parser"
        [
          ("literal", Literal.tests);
          ("ident_like", Ident_like.tests);
          ("list_of", List_of.tests);
        ];
    ]
