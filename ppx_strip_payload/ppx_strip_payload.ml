(* TODO: Double check this code *)

open Ppxlib
open Ast_builder.Default

(* 1. Helper to remove payload from a single constructor *)
let strip_constructor_decl cd =
  { cd with pcd_args = Pcstr_tuple []; pcd_res = None }

(* 2. Core generator function *)
let generate_stripped_type ~ctxt (rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  (* Map each input type declaration into a separate top-level structure item *)
  List.map
    (fun td ->
      match td.ptype_kind with
      | Ptype_variant constructors ->
          (* Pluck the name and make it singular *)
          let new_name =
            let orig = td.ptype_name.txt in
            if String.ends_with ~suffix:"s" orig then
              String.sub orig 0 (String.length orig - 1)
            else orig ^ "_stripped"
          in

          (* Strip payloads from constructors *)
          let stripped_constructors =
            List.map strip_constructor_decl constructors
          in

          (* Create the attribute node for: [@@deriving enumerate, show] *)
          let deriving_attr =
            let attr_name = Located.mk ~loc "deriving" in
            let attr_payload =
              PStr [%str enumerate, ord, show { with_path = false }]
            in
            attribute ~loc ~name:attr_name ~payload:attr_payload
          in

          (* Construct the new type declaration with the attributes injected *)
          let new_td =
            {
              (type_declaration ~loc ~name:(Located.mk ~loc new_name) ~params:[]
                 ~cstrs:[] ~kind:(Ptype_variant stripped_constructors)
                 ~private_:Public ~manifest:None)
              with
              ptype_attributes = [ deriving_attr ];
            }
          in

          (* WRAPPER FIX: Wrap the type declaration into a top-level structure item *)
          pstr_type ~loc rec_flag [ new_td ]
      | _ ->
          Location.raise_errorf ~loc "strip_payload only supports variant types")
    type_declarations

(* 3. Register the PPX as a standard @@deriving plugin *)
let str_type_decl = Deriving.Generator.V2.make_noarg generate_stripped_type

let () =
  let _ = Deriving.add "strip_payload" ~str_type_decl in
  ()
