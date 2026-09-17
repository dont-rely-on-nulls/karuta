open Result

type t =
  | CouldNotPreprocess of string
  | EmptyFilepath
  | EmptyFile of string
  | InvalidExtension of string
  | DependencyCycle of string list
  | FailedToRunExternalProgram of string * Unix.process_status

let process_status_message = function
  | Unix.WEXITED n -> "exited with code " ^ string_of_int n
  | Unix.WSIGNALED n | Unix.WSTOPPED n ->
      "interrupted by signal " ^ Sys.signal_to_string n

let display : t -> string = function
  | CouldNotPreprocess filepath -> "Could not preprocess file " ^ filepath
  | EmptyFilepath -> "Input filepath is empty"
  | EmptyFile file -> "Input file '" ^ file ^ "' is empty"
  | InvalidExtension file -> "Input file '" ^ file ^ "' has invalid extension"
  | DependencyCycle files ->
      "Dependency cycle detected across these files: "
      ^ String.concat ", " files
  | FailedToRunExternalProgram (filepath, process_status) ->
      "Failed to run " ^ filepath ^ "("
      ^ process_status_message process_status
      ^ ")\n"

type 'a attempt = ('a, t) result

let ok = ok
let error = error
let ( ||> ) = bind
let map = Result.map
let void (attempt : 'a attempt) : unit = Result.iter (Fun.const ()) attempt

let fold (f : 'b FT.t -> 'a -> 'b FT.t attempt) (elements : 'a FT.t) :
    'b FT.t attempt =
  FT.fold_left
    (fun acc element -> Result.bind acc (Fun.flip f element))
    (ok FT.empty) elements

let ( let* ) o f =
  match o with
  | Error err ->
      Logger.simply_error @@ display err;
      exit 1
  | Ok x -> f x
