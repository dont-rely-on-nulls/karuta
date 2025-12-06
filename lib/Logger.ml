module type OUTPUT = sig
  val format_error_message :
    string -> Location.location option -> string -> string

  val format_warning_message : string -> Location.location -> string -> string
  val format_info_message : string -> Location.location -> string -> string

  val format_unreachable_message :
    string -> Location.location option -> string -> string

  val format_debug_message : string -> string -> string
  val export : string -> unit
end

module Make (Output : OUTPUT) = struct
  type t = Warning | Info | Error | Unreachable | Debug

  let unreachable_suffix =
    "This is a compiler bug. Please report it in \
     https://github.com/dont-rely-on-nulls/karuta"

  let get_prefix : t -> string = function
    | Warning -> "[WARNING] "
    | Info -> "[INFO] "
    | Error -> "[ERROR] "
    | Debug -> "[DEBUG] "
    | Unreachable -> "[UNREACHABLE] " ^ unreachable_suffix ^ "\n"

  let rec format level locMay msg =
    let prefix = get_prefix level in
    match (level, locMay) with
    | Warning, Some loc -> Output.format_warning_message prefix loc msg
    | Info, Some loc -> Output.format_info_message prefix loc msg
    | Error, Some loc -> Output.format_error_message prefix (Some loc) msg
    | Unreachable, Some loc ->
        Output.format_unreachable_message prefix (Some loc) msg
    | _, _ -> create_simply_msg Unreachable "Output functor is broken"

  and report level locMay msg = msg |> format level locMay |> Output.export
  and create_simply_msg level msg = format level None msg

  and internal level (locMay : Location.location option) (msg : string) : unit =
    match locMay with
    | None -> msg |> create_simply_msg level |> print_endline
    | Some loc -> report Unreachable (Some loc) msg

  and unreachable (loc : Location.location) (msg : string) : unit =
    internal Unreachable (Some loc) msg

  and simply_unreachable (msg : string) : unit = internal Unreachable None msg

  and error (loc : Location.location) (msg : string) : unit =
    internal Error (Some loc) msg

  and simply_error (msg : string) : unit = internal Error None msg

  let warning loc msg = report Warning (Some loc) msg
  let info loc msg = report Info (Some loc) msg

  let debug (msg : string) : unit =
    let prefix = get_prefix Debug in
    print_endline @@ Output.format_debug_message prefix msg
end

module Terminal = Make ((
  struct
    open Format

    type color = Red | Yellow | White | Blue | Magenta

    let color_code : color -> string = function
      | Red -> "\027[31m"
      | Yellow -> "\027[33m"
      | White -> "\027[97m"
      | Blue -> "\027[34m"
      | Magenta -> "\027[35m"

    let make_bold (str : string) = "\027[1m" ^ str ^ "\027[0m"

    let add_color (c : color) (str : string) =
      color_code c ^ str ^ color_code White

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

    let format_message (color : color) (prefix : string)
        (loc : Location.location) (msg : string) : string =
      let filepath = loc.startl.pos_fname in
      let begin_characters = get_column loc.startl in
      let end_characters = get_column loc.endl in
      let how_many_characters = end_characters - (begin_characters + 1) in
      let line_number = loc.startl.pos_lnum in
      let line_digits : int = line_number |> string_of_int |> String.length in
      let spaces = String.make (3 + line_digits + begin_characters) ' ' in
      let markers =
        make_bold @@ add_color color
        @@ String.make (max how_many_characters 1) '^'
      in
      if how_many_characters = 0 then
        sprintf
          "@[<1>%sFile \"%s\", line %d, character %d:@]@.@[<1>%d | \
           %s@]@.@[<1>%s%s@]@."
          (add_color color prefix) filepath line_number end_characters
          line_number
          (get_line filepath (loc.startl.pos_cnum - begin_characters))
          spaces
          (markers ^ " " ^ msg)
      else
        sprintf
          "@[<1>%sFile \"%s\", line %d, characters %d-%d:@]@.@[<1>%d | \
           %s@]@.@[<1>%s%s@]@."
          (add_color color prefix) filepath line_number (begin_characters + 1)
          end_characters line_number
          (get_line filepath (loc.startl.pos_cnum - begin_characters))
          spaces
          (markers ^ " " ^ msg)

    let export = Stdlib.print_string

    let format_diverse_message (prefix : string) (color : color)
        (locMay : Location.location option) (msg : string) : string =
      match locMay with
      | None ->
          let prefix = prefix |> make_bold |> add_color color in
          prefix ^ msg
      | Some loc -> format_message Magenta prefix loc msg

    let format_error_message prefix locMay msg =
      format_diverse_message (make_bold prefix) Red locMay msg

    let format_unreachable_message (prefix : string)
        (locMay : Location.location option) (msg : string) : string =
      format_diverse_message (make_bold prefix) Magenta locMay msg

    let format_warning_message prefix loc msg =
      format_message Yellow (make_bold prefix) loc msg

    let format_info_message prefix loc msg =
      format_message Blue (make_bold prefix) loc msg

    let format_debug_message prefix msg =
      let color = White in
      let prefix = prefix |> make_bold |> add_color color in
      prefix ^ msg
  end :
    OUTPUT))

type kind = Terminal | Server

let current_kind = ref Terminal
let set kind = current_kind := kind

module type API = sig
  val error : Location.location -> string -> unit
  val simply_error : string -> unit
  val warning : Location.location -> string -> unit
  val unreachable : Location.location -> string -> unit
  val simply_unreachable : string -> unit
  val info : Location.location -> string -> unit
  val debug : string -> unit
end

let kind2Module () =
  match !current_kind with
  | Terminal -> (module Terminal : API)
  | Server ->
      Terminal.simply_unreachable
        "Server platform for logging is not implemented yet";
      exit 1

let simply_unreachable msg =
  let (module L) = kind2Module () in
  L.simply_unreachable msg

let unreachable loc msg =
  let (module L) = kind2Module () in
  L.unreachable loc msg

let simply_error msg =
  let (module L) = kind2Module () in
  L.simply_error msg

let error loc msg =
  let (module L) = kind2Module () in
  L.error loc msg

let warning loc msg =
  let (module L) = kind2Module () in
  L.warning loc msg

let info loc msg =
  let (module L) = kind2Module () in
  L.info loc msg

let debug msg =
  let (module L) = kind2Module () in
  L.debug msg
