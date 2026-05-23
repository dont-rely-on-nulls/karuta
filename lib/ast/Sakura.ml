type 'declaration t =
    Module of {name: string Location.with_location;
               signature: signature_ref option;
               declarations: multi_declaration Location.with_location list;
               directives: directive Location.with_location list;
               imports: string Location.with_location BatSet.t}
  | Signature of {name: string Location.with_location;
                  declarations: multi_declaration Location.with_location list;
                  directives: directive Location.with_location list}
  | Persisted of multi_declaration Location.with_location list
  | Ephemeral of multi_declaration Location.with_location list
  | Constraint of multi_declaration Location.with_location list


