open Lexing

type t = Lexing.position
(** Alias to standard library's Lexing.position type. Our type has the same
    fields as the original ones, which have the following description (official
    documentation):

    pos_fname is the file name; pos_lnum is the line number; pos_bol is the
    offset of the beginning of the line (number of characters between the
    beginning of the lexbuf and the beginning of the line); pos_cnum is the
    offset of the position (number of characters between the beginning of the
    lexbuf and the position). The difference between pos_cnum and pos_bol is the
    character offset within the line (i.e. the column number, assuming each
    character is one column wide). *)

type location = {
  startl : t;  (** Beginning of region *)
  endl : t;  (** End of region *)
}
(** Source code location. Locations are associated with source code region. *)

type 'a with_location = {
  content : 'a;  (** Generic type *)
  loc : location;  (** Associated location, with a beginning and end. *)
}
(** Parametric type to add location to any other type, although used for types
    that are associated with source code regions *)

(** [step n loc] advances offset position by provided amount.
    @param n amount to advance offset.
    @param loc location to be updated.
    @return updated location. *)
let step n loc = { loc with pos_cnum = loc.pos_cnum + n }

(** [jump loc] increments line number and resets beginning of the line offset.

    The reset is performed by updating pos_bol to be the provided location's
    pos_cnum.

    @param loc location to be updated.
    @return updated location. *)
let jump loc = { loc with pos_bol = loc.pos_cnum; pos_lnum = loc.pos_lnum + 1 }

(** [jump_n n loc] adds n to current line number and resets beginning of the
    line offset.

    If the provided n is zero, no changes are applied to the location. The reset
    is performed by updating pos_bol to be the provided location's pos_cnum.

    @param n amount to advance line number.
    @param loc location to be updated.
    @return updated location. *)
let jump_n n loc =
  if n = 0 then loc
  else { loc with pos_bol = loc.pos_cnum; pos_lnum = loc.pos_lnum + n }

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
let add p1 p2 v = { content = v; loc = { startl = p1; endl = p2 } }

(** [strip_loc v] Removes location from a value of type with location.
    @param v value of type with location.
    @return same value as before but without location. *)
let strip_loc (v : 'a with_location) : 'a = v.content

(** [add_loc v loc] Add full (beginning and end) location to a value of type
    without location.
    @param v value of type without location.
    @param loc location to be added.
    @return same value as before updated with provided location. *)
let add_loc (v : 'a) (loc : location) : 'a with_location = { content = v; loc }

(** [double loc] Receive half of a location and create a full (beginning and
    end) location.
    @param loc half-location to be used.
    @return full location. *)
let double (loc : t) : location = { startl = loc; endl = loc }

(** Full dummy location based on Lexing.position (half-location). *)
let dummy = double @@ Lexing.dummy_pos
