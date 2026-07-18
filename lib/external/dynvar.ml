(** Dynamically scoped variables in Ocaml 5.x and beyond, using effects Dynamic
    variables implemented here are safe with respect to exceptions *and*
    effects.

    If we implement a lightweight threading using OCaml effects, the dynamic
    variables here work as expected. Naively implemented dynamic variables will
    not: see ``Delimited Dynamic Binding'' with Chung-chieh Shan and Amr Sabry,
    ICFP 2006.

    Dynamic variables have a richer interface, as described in the above paper.
*)

open Effect

let execute : 'a -> 'b = fun _ -> assert false

type 'a dynvar = {
  init : 'a option;
  dref : unit -> 'a;
  dset : 'a -> 'a;
  dupp : 'w. ('a -> 'w) -> 'w;
  catch : 'w. 'a -> (unit -> 'w) -> 'w;
}

let dnew : type a. ?init:a -> unit -> a dynvar =
 fun ?init () ->
  let open struct
    type _ eff += Get : a eff | Set : a -> a eff | Up : 'w. (a -> 'w) -> 'w eff
  end in
  {
    init;
    dref =
      (fun () ->
        try perform Get with Unhandled _ when init <> None -> Option.get init);
    dset = (fun x -> perform (Set x));
    dupp = (fun g -> perform (Up g));
    catch =
      (fun x f ->
        x
        |>
        try f () |> Fun.const with
        | effect Get, k -> fun v -> (Deep.continue k (v : a)) v
        | effect Set vnew, k -> fun v -> (Deep.continue k (v : a)) vnew
        | effect Up g, k -> fun v -> (Deep.continue k (g v)) v);
  }

(* Dereferencing: obtain the value associated with the dynvar
   by the closest dynamically-enclosing dlet *)
let dref : 'a dynvar -> 'a = fun { dref } -> dref ()

(* Given a dynvar, a value and a thunk, evaluate the thunk in the dynamic
   environment extended with the binding of the given value to the
   given dynvar
*)
let dlet : 'a dynvar -> 'a -> (unit -> 'w) -> 'w = fun { catch } -> catch

(* Mutation: obtain the current value of the dynvar and change that
   value to the given one. This `mutation' has the effect only
   within the dynamic scope of the latest dlet
   Return the previous value.
*)
let dset : 'a dynvar -> 'a -> 'a = fun { dset } -> dset

(* Given a dynvar and a function, apply the function to the current
   value of the dynvar, and return the result. The application
   is evaluated in the dynamic environment _outside_ of the
   closest dynamically-enclosing dlet. This lets us, for example,
   access the previous binding to the dynamic variable.
*)
let dupp : 'a dynvar -> ('a -> 'w) -> 'w = fun { dupp } -> dupp
