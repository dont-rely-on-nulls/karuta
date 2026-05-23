type 'declaration signature_ref =
    Named of string Location.with_location
  | Inlined of {declarations: 'declaration Location.with_location list;
                directives: directive Location.with_location list}

and 'declaration t =
  Module of {name: string Location.with_location;
             signature: 'declaration signature_ref option;
             declarations: 'declaration Location.with_location list;
             directives: directive Location.with_location list;
             imports: string Location.with_location BatSet.t}
| Signature of {name: string Location.with_location;
                declarations: 'declaration Location.with_location list;
                directives: directive Location.with_location list}
