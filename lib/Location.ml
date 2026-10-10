type coordinate = {
  offset : int;
      (** Offset from the start of the file to the current position. *)
  line_offset : int;
      (** Offset from the start of the file to the start of the current line. *)
  line : int;  (** Line number. 1-based. *)
}
(** A coordinate in source code. It has no knowledge of which file it belongs
    to.

    It is possible to recover the column number by doing offset -
    offset_of_line. *)

let origin = { offset = 0; line_offset = 0; line = 1 }

type point = { filename : string; coordinate : coordinate }
(** This type represents a point in source code. It is isomorphic to
    Lexing.position, but with better names for the fields. *)

type region = {
  filename : string;  (** File name *)
  startl : coordinate;  (** Beginning of region *)
  endl : coordinate;  (** End of region *)
}
(** Source code location. Locations delimit a source code region. *)

type 'a with_location = {
  content : 'a;  (** Generic type payload *)
  loc : region;  (** Associated location, with a beginning and end. *)
}
(** Parametric type to add a location to any other type. Used to indicate the
    source code region where the payload originated. *)

(** [step n loc] advances offset position by provided amount.
    @param n amount to advance offset.
    @param loc location to be updated.
    @return updated location. *)
let step n coordinate = { coordinate with offset = coordinate.offset + n }

(** [jump loc] increments line number and resets beginning of the line offset.

    The reset is performed by updating pos_bol to be the provided location's
    pos_cnum.

    @param loc location to be updated.
    @return updated location. *)
let jump ({ coordinate; _ } as point) =
  {
    point with
    coordinate =
      {
        coordinate with
        line_offset = coordinate.offset;
        line = coordinate.line + 1;
      };
  }

(** [jump_n n loc] adds n to current line number and resets beginning of the
    line offset.

    If the provided n is zero, no changes are applied to the location. The reset
    is performed by updating pos_bol to be the provided location's pos_cnum.

    @param n amount to advance line number.
    @param loc location to be updated.
    @return updated location. *)
let jump_n n coordinate =
  if n = 0 then coordinate
  else
    {
      coordinate with
      line_offset = coordinate.offset;
      line = coordinate.line + n;
    }

(** [plus_str str loc] step and jump combined based on the provided str
    argument.

    We step through the location based on the length of the provide string. We
    then jump_n using the amount of new lines in the provided string as the
    amount to jump.

    @param str string to be inspected.
    @param loc location to be updated.
    @return updated location. *)
let plus_str str loc =
  step (String.length str) loc
  |> jump_n
       (String.fold_left (fun n -> function '\n' -> n + 1 | _ -> n) 0 str)

(** [fmap f v] Maps the contents of a type with location.

    Inspired on the covariant Functors.

    @param f function to be applied to contents.
    @param v value of a type with location.
    @return updated value with the same location as before. *)
let fmap f { content = a; loc } = { content = f a; loc }

(** [add p1 p2 v] Adds beginning and end locations to a value of type without
    location.
    @param p1 beginning of location.
    @param p2 end of location.
    @param v value of a type without location.
    @return updated value with new location. *)
let add { filename; coordinate = startl } { coordinate = endl; _ } v =
  { content = v; loc = { filename; startl; endl } }

(** [strip_loc v] Removes location from a value of type with location.
    @param v value of type with location.
    @return same value as before but without location. *)
let strip_loc (v : 'a with_location) : 'a = v.content

(** [add_loc v loc] Add full (beginning and end) location to a value of type
    without location.
    @param v value of type without location.
    @param loc location to be added.
    @return same value as before updated with provided location. *)
let add_loc (v : 'a) (loc : region) : 'a with_location = { content = v; loc }

(** [double loc] Receive half of a location and create a full location by using
    the argument as both beginning and end.
    @param loc half-location to be used.
    @return full location. *)
let double filename (loc : coordinate) : region =
  { filename; startl = loc; endl = loc }

(** Dummy region. Note that the line is 0 even though it is 1-based. *)
let dummy = double "" { offset = 0; line_offset = 0; line = 0 }
