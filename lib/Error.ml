open Result

type t =
  | CouldNotPreprocess of string
  | EmptyFilepath
  | EmptyFile of string
  | InvalidExtension of string
[@@deriving show, ord]

let display : t -> string = function
  | CouldNotPreprocess filepath -> "Could not preprocess file " ^ filepath
  | EmptyFilepath -> "Input filepath is empty"
  | EmptyFile file -> "Input file '" ^ file ^ "' is empty"
  | InvalidExtension file -> "Input file '" ^ file ^ "' has invalid extension"

type 'a attempt = ('a, t) result [@@deriving show, ord]

let ok = ok
let error = error
let ( ||> ) = bind

let ( let* ) o f =
  match o with
  | Error err ->
      Logger.simply_error @@ display err;
      exit 1
  | Ok x -> f x
