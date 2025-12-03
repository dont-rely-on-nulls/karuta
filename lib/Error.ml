open Result

type t = CouldNotPreprocess of string | CouldNotParse of string | InvalidFile
[@@deriving show, ord]

let display : t -> string = function
  | CouldNotPreprocess filepath -> "Could not preprocess file " ^ filepath
  | CouldNotParse filepath -> "Could not parse file" ^ filepath
  | InvalidFile -> "Input file is empty"

type 'a attempt = ('a, t) result [@@deriving show, ord]

let ok = ok
let error = error
let ( ||> ) = bind
