open Format

type t = Warning | Info | Error | Unreachable | Debug
type color = Red | Yellow | White | Blue | Magenta

let get_color : t -> color = function
  | Warning -> Yellow
  | Info -> White
  | Error -> Red
  | Unreachable -> Magenta
  | Debug -> Blue

let color_code : color -> string = function
  | Red -> "\027[31m"
  | Yellow -> "\027[33m"
  | White -> "\027[97m"
  | Blue -> "\027[34m"
  | Magenta -> "\027[35m"

let unreachable_suffix =
  "This is a compiler bug. Please report it in \
   https://github.com/dont-rely-on-nulls/karuta"

let get_prefix : t -> string = function
  | Warning -> "[WARNING] "
  | Info -> "[INFO] "
  | Error -> "[ERROR] "
  | Debug -> "[DEBUG] "
  | Unreachable -> "[UNREACHABLE] " ^ unreachable_suffix ^ "\n"

let make_bold (str : string) = "\027[1m" ^ str ^ "\027[0m"
let add_color (c : color) (str : string) = color_code c ^ str ^ color_code White

let get_column ({ pos_cnum; pos_bol; _ } : Location.t) : int =
  pos_cnum - pos_bol

let get_line (filepath : string) (line : int) : string =
  let input_channel = open_in filepath in
  try
    seek_in input_channel line;
    let line = input_line input_channel in
    close_in input_channel;
    line
  with exp ->
    close_in_noerr input_channel;
    raise exp

let report (level : t) (loc : Location.location) (msg : string) : unit =
  let prefix = make_bold @@ get_prefix level in
  let filepath = loc.startl.pos_fname in
  let begin_characters = get_column loc.startl in
  let end_characters = get_column loc.endl in
  let how_many_characters = end_characters - (begin_characters + 1) in
  let line_number = loc.startl.pos_lnum in
  let line_digits : int = line_number |> string_of_int |> String.length in
  let color = get_color level in
  let spaces = String.make (3 + line_digits + begin_characters) ' ' in
  let markers =
    make_bold @@ add_color color @@ String.make (max how_many_characters 1) '^'
  in
  if how_many_characters = 0 then
    printf
      "@[<1>%sFile \"%s\", line %d, character %d:@]@.@[<1>%d | \
       %s@]@.@[<1>%s%s@]@."
      (add_color color prefix) filepath line_number end_characters line_number
      (get_line filepath (loc.startl.pos_cnum - begin_characters))
      spaces
      (markers ^ " " ^ msg)
  else
    printf
      "@[<1>%sFile \"%s\", line %d, characters %d-%d:@]@.@[<1>%d | \
       %s@]@.@[<1>%s%s@]@."
      (add_color color prefix) filepath line_number (begin_characters + 1)
      end_characters line_number
      (get_line filepath (loc.startl.pos_cnum - begin_characters))
      spaces
      (markers ^ " " ^ msg)

let error (loc : Location.location) (msg : string) : unit = report Error loc msg

let warning (loc : Location.location) (msg : string) : unit =
  report Warning loc msg

let internal_unreachable (locMay : Location.location option) (msg : string) :
    unit =
  match locMay with
  | None ->
      let color = get_color Unreachable in
      let prefix = Unreachable |> get_prefix |> make_bold |> add_color color in
      print_endline @@ prefix ^ msg
  | Some loc -> report Unreachable loc msg

let unreachable (loc : Location.location) (msg : string) : unit =
  internal_unreachable (Some loc) msg

let simply_unreachable (msg : string) : unit = internal_unreachable None msg
let info (loc : Location.location) (msg : string) : unit = report Info loc msg

let debug (msg : string) : unit =
  let color = get_color Debug in
  let prefix = Debug |> get_prefix |> make_bold |> add_color color in
  print_endline @@ prefix ^ msg
