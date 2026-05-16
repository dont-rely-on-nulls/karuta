type directive = Persisted | Ephemeral | Constraint | Stored
[@@deriving enumerate, show, ord]

(* TODO: replace string with a proper type type (sic) *)
type relation_signature = string BatMap.String.t
type relations = relation_signature BatMap.String.t

module Schema = BatMap.Make (struct
  type t = directive [@@deriving show, ord]
end)

type inner_state = {
  schemas : relations Schema.t BatMap.String.t;
  current_directive : directive Location.with_location option;
}

type state = inner_state Atomic.t

let initial_state () =
  Atomic.make { schemas = BatMap.String.empty; current_directive = None }
