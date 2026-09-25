open Helpers

let tests : (string * Outcome.t BatMap.String.t) list =
  [
    ("Arithmetic", Arithmetic.tests);
    ("Basic", Basic.tests);
    ("Lists", Lists.tests);
    ("Module System", Module_system.tests);
  ]
