type directive = Persisted | Ephemeral | Constraint | Stored
[@@deriving enumerate, show, ord]

let supported_directives =
  BatSet.String.of_list
    (List.map
       (Fun.compose String.lowercase_ascii show_directive)
       all_of_directive)

let formatted_supported_directives =
  String.concat ", " @@ BatSet.String.to_list supported_directives

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

type mods = unit

type directives =
  | Persisted of Ast.Clause.multi_declaration Location.with_location FT.t
  | Ephemeral of Ast.Clause.multi_declaration Location.with_location FT.t
  | Constraint of Ast.Clause.multi_declaration Location.with_location FT.t
