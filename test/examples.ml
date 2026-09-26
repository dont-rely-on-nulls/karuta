open Integration

let tests : (string * Outcome.t BatMap.String.t) list =
  [
    ("Examples - Arithmetic", Arithmetic.tests);
    ("Examples - Basic", Basic.tests);
    ("Examples - Lists", Lists.tests);
    ("Examples - Module System", Module_system.tests);
  ]
