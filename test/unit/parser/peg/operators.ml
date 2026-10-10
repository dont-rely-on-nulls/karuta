open Lib.Parser
open Alcotest

let atandand_success () =
  let l input = Ok ("discard me", input + 1) in
  let r state = Ok (state * 2) in

  let actual = (l @&& r) 10 in

  check (result int string) "returns result of r" (Ok 22) actual

let atandand_does_not_run_right_on_failure () =
  let l _ = Error "left failed" in
  let r _ = failwith "r should not be called" in

  let actual = (l @&& r) 10 in

  check (result int string) "propagates l's error" (Error "left failed") actual

let tests =
  [
    test_case "@&& success" `Quick atandand_success;
    test_case "@&& does not run right on failure" `Quick
      atandand_does_not_run_right_on_failure;
  ]
