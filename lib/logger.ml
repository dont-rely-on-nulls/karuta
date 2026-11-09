(** Level module only handles the logger's level type and its defaul value.

    This is used for different log levels across the compiler. *)
module Level = struct
  (** Debug log level is suppose to never appear in production use of the
      compiler *)
  type t = Debug | Info | Warning | Error | Unreachable
  [@@deriving ord, enumerate, show { with_path = false }]

  (** [unreachable_prefix] where to report unreachable logs. *)
  let unreachable_suffix =
    "This is a compiler bug. Please report it in \
     https://github.com/dont-rely-on-nulls/karuta"

  (** [get_prefix level] gives you which prefix for a logger message for that
      log level.

      @param level Log level to be logged.
      @return Prefix string. *)
  let get_prefix : t -> string = function
    | Warning -> "[WARNING] "
    | Info -> "[INFO] "
    | Error -> "[ERROR] "
    | Debug -> "[DEBUG] "
    | Unreachable -> "[UNREACHABLE] " ^ unreachable_suffix ^ "\n"

  (** [default] holds the initial current value for the logger. This can be
      changed throughout execution. *)
  let default = Info
end

(** Because the logger is actually a functor, this signature tells you what is
    required in order to create a Logger module.

    This input modules exclusively handles message formatting. *)
module type OUTPUT = sig
  val format_message : Level.t -> Location.region option -> string -> string
  (** [format_message level locMay message] formats the provided [msg] with the
      prefix for log [level]. Logging a message may optionally have a location
      [locMay].

      @param level Log level of the message to be logged.
      @param locMay
        Optional location to be reported by the logger alongside the message.
      @param msg Message to be logged.
      @return Formatted string. *)
end

(** Second argument for the logger functor: control over message addition, which
    file the logger is in scope for, and message dispatching. This is required
    for concurrency-safe logging.

    WRITE and OUTPUT are the two modules you gotta create in order to use the
    logger. Writing operations got separated to allow unit testing with the
    logger; allow writing operations of the logger to not be locked to always
    effectful functions. *)
module type WRITE = sig
  val in_file : string -> (unit -> 'a) -> 'a
  (** [in_file filepath continuation] puts the logger in the "scope" of filepath
      with the continuation argument to proceed. This is only used for
      concurrency-safe logging.

      @param filepath
        Provides which filepath the current thread will be in scope for.
      @param continuation Function to execute.
      @return run the logger with the new scope set. *)

  val export : string -> unit
  (** [export msg] adds the provided message to the current logger context.
      Different contexts only matter for concurrency-safe logging.

      @param msg Message to be logged, but only when flush happens.
      @return saves the message. *)

  val flush : unit -> unit
  (** [flush ()] simply flushes all the messages saved in context. We use Dict
      as our concurrency-safe logger pool of messages.

      @param unit
      @return
        flushes all the existing saved messages according to implementation. *)
end

(** Module signature of what the logger functor should provide.

    This should be the only set of functions the compiler use. *)
module type API = sig
  val error : Location.region -> string -> unit
  (** [error loc msg] logs [msg] with location [loc] with log level Error.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)

  val simply_error : string -> unit
  (** [simply_error msg] logs [msg] with log level Error.

      @param msg Message to be logged.
      @return saves message into logger context. *)

  val warning : Location.region -> string -> unit
  (** [warning loc msg] logs [msg] with location [loc] with log level Warning.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)

  val simply_warning : string -> unit
  (** [simply_warning msg] logs [msg] with log level Warning.

      @param msg Message to be logged.
      @return saves message into logger context. *)

  val unreachable : Location.region -> string -> unit
  (** [unreachable loc msg] logs [msg] with location [loc] with log level
      Unrechable.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)

  val simply_unreachable : string -> unit
  (** [simply_unreachable msg] logs [msg] with log level Unrechable.

      @param msg Message to be logged.
      @return saves message into logger context. *)

  val info : Location.region -> string -> unit
  (** [info loc msg] logs [msg] with location [loc] with log level Info.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)

  val simply_info : string -> unit
  (** [simply_info msg] logs [msg] with log level Info.

      @param msg Message to be logged.
      @return saves message into logger context. *)

  val debug : Location.region -> string -> unit
  (** [debug loc msg] logs [msg] with location [loc] with log level Debug.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)

  val simply_debug : string -> unit
  (** [simply_debug msg] logs [msg] with log level Debug.

      @param msg Message to be logged.
      @return saves message into logger context. *)

  val with_min_level : 'a. Level.t -> (unit -> 'a) -> 'a
  (** [with_min_level level continuation] updates the log level for the lifetime
      of the provided continuation.

      @param level Desired log level to be used as minimum in continuation.
      @param continuation Function to run with reset log level.
      @return Executes the continuation after the reset. *)

  val flush : unit -> unit
  (** [flush ()] simply flushes all the messages saved in context. We use Dict
      as our concurrency-safe logger pool of messages.

      @param unit
      @return
        flushes all the existing saved messages according to implementation. *)

  val in_file : string -> (unit -> 'a) -> 'a
  (** [in_file filepath continuation] puts the logger in the "scope" of filepath
      with the continuation argument to proceed. This is only used for
      concurrency-safe logging.

      @param filepath
        Provides which filepath the current thread will be in scope for.
      @param continuation Function to execute.
      @return run the logger with the new scope set. *)
end

(** Functor to create a logger out of OUTPUT and WRITE primitives.

    The implementation does not assume logging will happen on stdout or
    stderror; that is up to the implementor of OUTPUT to decide. Same rationale
    for WRITE. *)
module Make (Output : OUTPUT) (Write : WRITE) : API = struct
  open Level

  (** [flush] simply uses Output provided flush. *)
  let flush = Write.flush

  (** [in_file] simply uses Output provided in_file. *)
  let in_file = Write.in_file

  (** [current_min_level] internal-only minimum level control. This provides
      concurrency-safe log level control via the use of fibers. It is not
      supposed to be used throughout the compiler. *)
  let current_min_level = Eio.Fiber.create_key ()

  (** [get_min_level ()] internal-only minimum level getter.

      @param unit
      @return
        Reads the current minimum log level using default log level as fallback.
  *)
  let get_min_level () =
    Eio.Fiber.get current_min_level
    |> Option.fold ~none:Level.default ~some:Fun.id

  (** [with_min_level level continuation] updates the log level for the lifetime
      of the provided continuation.

      @param level Desired log level to be used as minimum in continuation.
      @param continuation Function to run with reset log level.
      @return Executes the continuation after the reset. *)
  let with_min_level : 'a. Level.t -> (unit -> 'a) -> 'a =
   fun level -> Eio.Fiber.with_binding current_min_level level

  (** [should_log level] helper that checks if we are allowed to log given the
      current log level in the current context.

      @param level Desired log level to be used.
      @return boolean is report is allowed to run. *)
  let should_log level = Level.compare (get_min_level ()) level <= 0

  (** [report level locMay msg] exports the formatted [msg] on log level [level]
      with an optional location [locMay].

      @param level Desired log level to be used.
      @param locMay
        Optional location to be reported by the logger alongside the message.
      @param msg Message to be logged.
      @return Runs formatted message. *)
  let report level locMay msg =
    msg |> Output.format_message level locMay |> Write.export

  (** [internal level locMay msg] conditionally reports [msg] for log level
      [level] and [locMay] based on the desired log level and the current
      minimum log level.

      @param level Desired log level to be used.
      @param locMay
        Optional location to be reported by the logger alongside the message.
      @param msg Message to be logged.
      @return
        Report message only if provided level is above or equal to minimum. *)
  let internal level locMay msg =
    if should_log level then report level locMay msg

  (** [unreachable loc msg] logs [msg] with location [loc] with log level
      Unrechable.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)
  let unreachable loc = internal Unreachable (Some loc)

  (** [simply_unreachable msg] logs [msg] with log level Unrechable.

      @param msg Message to be logged.
      @return saves message into logger context. *)
  let simply_unreachable = internal Unreachable None

  (** [error loc msg] logs [msg] with location [loc] with log level Error.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)
  let error loc = internal Warning (Some loc)

  (** [simply_error msg] logs [msg] with log level Error.

      @param msg Message to be logged.
      @return saves message into logger context. *)
  let simply_error = internal Error None

  (** [warning loc msg] logs [msg] with location [loc] with log level Warning.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)
  let warning loc = internal Warning (Some loc)

  (** [simply_warning msg] logs [msg] with log level Warning.

      @param msg Message to be logged.
      @return saves message into logger context. *)
  let simply_warning = internal Warning None

  (** [info loc msg] logs [msg] with location [loc] with log level Info.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)
  let info loc = internal Info (Some loc)

  (** [simply_info msg] logs [msg] with log level Info.

      @param msg Message to be logged.
      @return saves message into logger context. *)
  let simply_info = internal Info None

  (** [debug loc msg] logs [msg] with location [loc] with log level Debug.

      @param loc Location to be reported alongside message.
      @param msg Message to be logged.
      @return saves message into logger context. *)
  let debug loc = internal Debug (Some loc)

  (** [simply_debug msg] logs [msg] with log level Debug.

      @param msg Message to be logged.
      @return saves message into logger context. *)
  let simply_debug = internal Debug None
end

(** Currently this is the single available output for the logger functor.

    This implementation assumes a terminal-like environment. *)
module Terminal : OUTPUT = struct
  (** In the terminal, we use different colors for immediate differentiation
      between log levels. *)
  type color = Red | Yellow | Blue | Magenta | Foreground

  (** [color_code color] determines what is the color code for the terminal
      given the provided color.

      @param color Color to identify the color.
      @return string of the color code. *)
  let color_code : color -> string = function
    | Red -> "\027[31m"
    | Yellow -> "\027[33m"
    | Foreground -> "\027[0m"
    | Blue -> "\027[34m"
    | Magenta -> "\027[35m"

  (** [level_color color] determines what is the color code for the terminal
      given the provided color.

      @param color Color to identify the color.
      @return string of the color code. *)
  let level_color : Level.t -> color = function
    | Warning -> Red
    | Info -> Blue
    | Error -> Red
    | Debug -> Foreground
    | Unreachable -> Magenta

  (** [make_bold str] makes the [str] bold in the terminal.

      @param str String to make bold via wrapping it with codes.
      @return bold string. *)
  let make_bold (str : string) = "\027[1m" ^ str ^ "\027[0m"

  (** [add_color c str] surrounds [str] with the color codes of [c].

      @param c Color type.
      @param str String to colorize using color code.
      @return colored string. *)
  let add_color (c : color) (str : string) =
    color_code c ^ str ^ color_code Foreground

  (** [get_column c] calculate the column value from [c] coordinate.

      @param c Our custom Location.coordiante type.
      @return column value of current line. *)
  let get_column ({ offset; line_offset; _ } : Location.coordinate) : int =
    offset - line_offset

  (** [get_line filepath line] retrieves the line from the provided file.

      @param filepath Filepath to read.
      @param line Line number to be retrieved.
      @return read line from file. *)
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

  (** [prepare_message color prefix loc msg] constructs formatted [prefix] with
      [msg], with [loc]. It uses Format module to facilitate formatting of the
      messages with location.

      @param color Color to be used in message formatting.
      @param prefix String to be added as prefix.
      @param loc Location to be reported.
      @param msg Message to be logged.
      @return message formatted with color and prefix. *)
  let prepare_message (color : color) (prefix : string) (loc : Location.region)
      (msg : string) : string =
    let filepath = loc.filename in
    let begin_characters = get_column loc.startl in
    let end_characters = get_column loc.endl in
    let separator = " | " in
    let how_many_characters = end_characters - begin_characters in
    let line_number = loc.startl.line in
    let line_digits : int = line_number |> string_of_int |> String.length in
    let spaces =
      String.make (line_digits + begin_characters + String.length separator) ' '
    in
    let markers =
      make_bold @@ add_color color
      @@ String.make (max how_many_characters 1) '^'
    in
    let open Format in
    if how_many_characters = 1 then
      sprintf
        "@[<1>%sFile \"%s\", line %d, character \
         %d:@]@.@[<1>%d%s%s@]@.@[<1>%s%s@]@."
        (add_color color prefix) filepath line_number end_characters line_number
        separator
        (get_line filepath (loc.startl.offset - begin_characters))
        spaces
        (markers ^ " " ^ msg)
    else
      sprintf
        "@[<1>%sFile \"%s\", line %d, characters \
         %d-%d:@]@.@[<1>%d%s%s@]@.@[<1>%s%s@]@."
        (add_color color prefix) filepath line_number (begin_characters + 1)
        end_characters line_number separator
        (get_line filepath (loc.startl.offset - begin_characters))
        spaces
        (markers ^ " " ^ msg)

  (** [format_message level locMay msg] formats [msg] for log level [level],
      with optional location [locMay].

      @param level Desired log level to be used.
      @param locMay
        Optional location to be reported by the logger alongside the message.
      @param msg Message to be logged.
      @return message formatted with provided log level. *)
  let format_message (level : Level.t) (locMay : Location.region option)
      (msg : string) : string =
    let prefix = Level.get_prefix level in
    let color = level_color level in
    match locMay with
    | None ->
        let prefix = prefix |> make_bold |> add_color color in
        prefix ^ msg
    | Some loc -> prepare_message color prefix loc msg
end

(** Currently this is the single available write for the logger functor.

    This implementation assumes a concurrent scenario of accumulating log
    messages to be flushed later via stdout. This implementation uses the Dict
    functor. *)
module Log : WRITE = struct
  type key = string
  (** Required for Dict implementation: our key is the filename string *)

  type value = string FT.t
  (** Required for Dict implementation: our value is a collection of messages *)

  (** Uses of Dict functor for multiple dictionary. *)
  module Internal = Dict.Make (struct
    type nonrec key = key
    (** Required for Dict implementation *)

    type nonrec value = value
    (** Required for Dict implementation *)

    (** [empty_value] empty state for our value is an empty collection. *)
    let empty_value = FT.empty

    (** [key_value_flush] implements a single key-value pair flush procedure. *)
    let key_value_flush _ msg = FT.iter Stdlib.print_endline msg
  end)

  (** [in_file filepath continuation] puts the logger in the "scope" of filepath
      with the continuation argument to proceed. This is only used for
      concurrency-safe logging. This leverages the Dict functor implementation.

      @param filepath
        Provides which filepath the current thread will be in scope for.
      @param continuation Function to execute.
      @return run the logger with the new scope set. *)
  let in_file = Internal.with_key

  (** [export msg] adds the provided message to the current logger context.
      Different contexts only matter for concurrency-safe logging. This
      implementation appends the provided message to our collection.

      @param msg Message to be logged, but only when flush happens.
      @return saves the message. *)
  let export msg = Internal.update @@ Fun.flip FT.snoc msg

  (** [flush ()] simply flushes all the messages saved in context. We use Dict
      as our concurrency-safe logger pool of messages. This leverages the Dict
      functor implementation.

      @param unit
      @return
        flushes all the existing saved messages according to implementation. *)
  let flush = Internal.flush_iter
end

(** [current] holds the current logger. This allows to change throughout
    execution. *)
let current = Eio.Fiber.create_key ()

(** [with_logger logger continuation] updates the logger for the lifetime of the
    provided continuation.

    @param logger Desired logger to be used in continuation.
    @param continuation Function to run with reset log level.
    @return Executes the continuation after the reset. *)
let with_logger logger = Eio.Fiber.with_binding current logger

module Default_Logger = Make (Terminal) (Log)
(** Default logger uses both Terminal and Log for its OUTPUT and WRITE
    arguments.

    This is the only logger implementation so far. *)

(** [get_current_module ()] internal-only current logger getter.
    @param unit
    @return Reads the current logger using default loggger as fallback. *)
let get_current_module () =
  Eio.Fiber.get current
  |> Option.fold ~none:(module Default_Logger : API) ~some:Fun.id

(** [simply_unreachable msg] logs [msg] with log level Unrechable in the current
    logger.

    @param msg Message to be logged.
    @return saves message into logger context. *)
let simply_unreachable msg =
  let (module L) = get_current_module () in
  L.simply_unreachable msg

(** [unreachable loc msg] logs [msg] with location [loc] with log level
    Unrechable in the current logger.

    @param loc Location to be reported alongside message.
    @param msg Message to be logged.
    @return saves message into logger context. *)
let unreachable loc msg =
  let (module L) = get_current_module () in
  L.unreachable loc msg

(** [simply_error msg] logs [msg] with log level Error in the current logger.

    @param msg Message to be logged.
    @return saves message into logger context. *)
let simply_error msg =
  let (module L) = get_current_module () in
  L.simply_error msg

(** [error loc msg] logs [msg] with location [loc] with log level Error in the
    current logger.

    @param loc Location to be reported alongside message.
    @param msg Message to be logged.
    @return saves message into logger context. *)
let error loc msg =
  let (module L) = get_current_module () in
  L.error loc msg

(** [simply_warning msg] logs [msg] with log level Warning in the current
    logger.

    @param msg Message to be logged.
    @return saves message into logger context. *)
let simply_warning msg =
  let (module L) = get_current_module () in
  L.simply_warning msg

(** [warning loc msg] logs [msg] with location [loc] with log level Warning in
    the current logger.

    @param loc Location to be reported alongside message.
    @param msg Message to be logged.
    @return saves message into logger context. *)
let warning loc msg =
  let (module L) = get_current_module () in
  L.warning loc msg

(** [simply_info msg] logs [msg] with log level Info in the current logger.

    @param msg Message to be logged.
    @return saves message into logger context. *)
let simply_info msg =
  let (module L) = get_current_module () in
  L.simply_info msg

(** [info loc msg] logs [msg] with location [loc] with log level Info in the
    current logger.

    @param loc Location to be reported alongside message.
    @param msg Message to be logged.
    @return saves message into logger context. *)
let info loc msg =
  let (module L) = get_current_module () in
  L.info loc msg

(** [simply_debug msg] logs [msg] with log level Debug in the current logger.

    @param msg Message to be logged.
    @return saves message into logger context. *)
let simply_debug msg =
  let (module L) = get_current_module () in
  L.simply_debug msg

(** [debug loc msg] logs [msg] with location [loc] with log level Debug in the
    current logger.

    @param loc Location to be reported alongside message.
    @param msg Message to be logged.
    @return saves message into logger context. *)
let debug ?pos_pair msg =
  let (module L) = get_current_module () in
  L.debug (Location.region_of_lexing @@ Option.get pos_pair) msg

(** [flush ()] simply flushes all the messages saved in context in the current
    logger.

    @param unit
    @return flushes all the existing saved messages according to implementation.
*)
let flush () =
  let (module L) = get_current_module () in
  L.flush ()

(** [in_file filepath continuation] puts the logger in the "scope" of filepath
    with the continuation argument to proceed in the current logger.

    @param filepath
      Provides which filepath the current thread will be in scope for.
    @param continuation Function to execute.
    @return run the logger with the new scope set. *)
let in_file filename =
  let (module L) = get_current_module () in
  L.in_file filename

(** [with_min_level level continuation] updates the log level for the lifetime
    of the provided continuation in the current logger.

    @param level Desired log level to be used as minimum in continuation.
    @param continuation Function to run with reset log level.
    @return Executes the continuation after the reset. *)
let with_min_level level =
  let (module L) = get_current_module () in
  L.with_min_level level
