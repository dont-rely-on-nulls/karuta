(** Dynamically scoped variables in Ocaml 5.x and beyond, using effects Dynamic
    variables implemented here are safe with respect to exceptions *and*
    effects.

    If we implement a lightweight threading using OCaml effects, the dynamic
    variables here work as expected. Naively implemented dynamic variables will
    not: see ``Delimited Dynamic Binding'' with Chung-chieh Shan and Amr Sabry,
    ICFP 2006.

    Dynamic variables have a richer interface, as described in the above paper.
*)

(* The abstract type of dynamic variables *)
type 'a dynvar

(* Create a new dynvar, optionally setting the default value *)
val dnew : ?init:'a -> unit -> 'a dynvar

(* Given a dynvar, a value and a thunk, evaluate the thunk in the dynamic
   environment extended with the binding of the given value to the
   given dynvar
*)
val dlet : 'a dynvar -> 'a -> (unit -> 'w) -> 'w

(* Dereferencing: obtain the value associated with the dynvar
   by the closest dynamically-enclosing dlet *)
val dref : 'a dynvar -> 'a

(* Mutation: obtain the current value of the dynvar and change that
   value to the given one. This `mutation' has the effect only
   within the dynamic scope of the latest dlet
*)
val dset : 'a dynvar -> 'a -> 'a

(* Given a dynvar and a function, apply the function to the current
   value of the dynvar, and return the result. The application
   is evaluated in the dynamic environment _outside_ of the
   closest dynamically-enclosing dlet. This lets us, for example,
   access the previous binding to the dynamic variable.
*)
val dupp : 'a dynvar -> ('a -> 'w) -> 'w
