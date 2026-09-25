type expected = (string * string) list list

let print_string_set =
  BatIO.to_string
    (BatSet.String.print ~first:"" ~last:"" ~sep:" " BatString.print)

let pp_value ppf v = Format.pp_print_string ppf (print_string_set v)
let batset_string = Alcotest.testable pp_value BatSet.String.equal

type t = {
  filepaths : string list;
  root_module : string;
  expected : expected;
  limit : int option;
}
