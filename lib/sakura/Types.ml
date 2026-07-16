type directives =
  | Persisted of Ast.Module.declaration_env
  | Ephemeral of Ast.Module.multi_declaration_env
  | Constraint of Ast.Module.multi_declaration_env
[@@deriving strip_payload]

let supported_directives =
  BatSet.String.of_list
    (List.map
       (Fun.compose String.lowercase_ascii show_directive)
       all_of_directive)

let formatted_supported_directives =
  String.concat ", "
  @@ List.map (fun directive_str -> "'" ^ directive_str ^ "'")
  @@ BatSet.String.to_list supported_directives

(* TODO: replace string with a proper type type (sic) *)
type argument_type = String
type relation_signature = argument_type BatMap.String.t
type relations = relation_signature BatMap.String.t

module Schema = BatMap.Make (struct
  type t = directive [@@deriving show, ord]
end)

type inner_state = { schemas : relations Schema.t BatMap.String.t }
type state = inner_state Atomic.t

let init_state () = Atomic.make { schemas = BatMap.String.empty }

type mods = unit

let merge_state (_ : mods) = Fun.id
