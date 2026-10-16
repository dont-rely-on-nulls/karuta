open Integration

let tests : (string * Outcome.t BatMap.String.t) list =
  Helpers.add_section_name "Examples"
    [
      ("Arithmetic", Arithmetic.tests);
      ("Basic", Basic.tests);
      ("Lists", Lists.tests);
      ("Module System", Module_system.tests);
    ]
