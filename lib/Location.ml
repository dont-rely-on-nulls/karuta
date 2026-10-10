type coordinate = {
  offset : int;
      (** Offset from the start of the file to the current position. *)
  line_offset : int;
      (** Offset from the start of the file to the start of the current line. *)
  line : int;  (** Line number. 1-based. *)
}
(** A coordinate in source code. It has no knowledge of which file it belongs
    to.

    It is possible to recover the column number by doing offset - line_offset.
*)

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

(** [start_point region] converts a region to a point using startl.
    @param region region to be converted
    @return point using startl. *)
let start_point { filename; startl; _ } = { filename; coordinate = startl }

(** [end_point region] converts a region to a point using endl.
    @param region region to be converted
    @return point using endl. *)
let end_point { filename; endl; _ } = { filename; coordinate = endl }

type 'a with_location = {
  content : 'a;  (** Generic type payload *)
  loc : region;  (** Associated location, with a beginning and end. *)
}
(** Parametric type to add a location to any other type. Used to indicate the
    source code region where the payload originated. *)

(** [step n coordinate] advances offset position by provided amount.
    @param n amount to advance offset.
    @param coordinate coordinate to be updated.
    @return updated coordinate. *)
let step n coordinate = { coordinate with offset = coordinate.offset + n }

(** [jump_n n coordinate] adds n to the line number and resets the line offset.

    If the provided n is zero, no changes are applied to the coordinate. The
    reset is performed by updating line_offset to be the provided location's
    offset.

    @param n amount to advance line number.
    @param coordinate to be updated.
    @return updated coordinate. *)
let jump_n n coordinate =
  if n = 0 then coordinate
  else
    {
      coordinate with
      line_offset = coordinate.offset;
      line = coordinate.line + n;
    }

(** [plus_str str coordinate] step and jump combined based on the provided str
    argument.

    We step the coordinate based on the length of the provided string. We then
    jump_n using the number of new lines in the provided string.

    @param str string to be inspected.
    @param coordinate to be updated.
    @return updated coordinate. *)
let plus_str str coordinate =
  coordinate
  |> step (String.length str)
  |> jump_n
       (String.fold_left (fun n -> function '\n' -> n + 1 | _ -> n) 0 str)

(** [fmap f v] Maps the contents of a type with location.

    Based on covariant Functors.

    @param f function to be applied to contents.
    @param v value of a type with location.
    @return updated value with the same location as before. *)
let fmap f { content = a; loc } = { content = f a; loc }

(** [delimit p1 p2 v] adds a region to v given its start and end points.

    @param p1 beginning of region.
    @param p2 end of region.
    @param v value without location.
    @return value with location. *)
let delimit { filename; coordinate = startl } { coordinate = endl; _ } v =
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

(** Dummy coordinate. Note that the line is 0 even though it is 1-based.*)
let half_dummy = { offset = 0; line_offset = 0; line = 0 }

(** Dummy region. *)
let dummy = double "" half_dummy

(** [dummy_coord_to_point coordinate] Receive a considered dummy coordinate and
    lift to a dummy point.
    @param coordinate coordinated assumed to be dummy.
    @return point with empty filename. *)
let dummy_coord_to_point (coordinate : coordinate) : point =
  { filename = ""; coordinate }

(** Zero coordinate. *)
let zero_coordinate = origin

(** Zero point. *)
let zero_point = dummy_coord_to_point zero_coordinate
