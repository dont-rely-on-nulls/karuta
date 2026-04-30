open Result

type t =
  | CouldNotPreprocess of string
  | EmptyFilepath
  | EmptyFile of string
  | InvalidExtension of string
  | DependencyCycle of string list
[@@deriving show, ord]

let display : t -> string = function
  | CouldNotPreprocess filepath -> "Could not preprocess file " ^ filepath
  | EmptyFilepath -> "Input filepath is empty"
  | EmptyFile file -> "Input file '" ^ file ^ "' is empty"
  | InvalidExtension file -> "Input file '" ^ file ^ "' has invalid extension"
  | DependencyCycle files ->
      "Dependency cycle detected across these files: "
      ^ String.concat ", " files

type 'a attempt = ('a, t) result [@@deriving show, ord]

let ok = ok
let error = error
let ( ||> ) = bind
let map = Result.map

let fold (f : 'b list -> 'a -> 'b list attempt) (elements : 'a list) :
    'b list attempt =
  List.fold_left
    (fun acc element -> Result.bind acc (Fun.flip f element))
    (ok []) elements

let ( let* ) o f =
  match o with
  | Error err ->
      Logger.simply_error @@ display err;
      exit 1
  | Ok x -> f x
