type 'a t = { promise : unit Eio.Promise.t; thunk : 'a Eio.Lazy.t }

let is_done { promise; _ } = Eio.Promise.is_resolved promise

let from_val v =
  { promise = Eio.Promise.create_resolved (); thunk = Eio.Lazy.from_val v }

let from_fun f =
  let promise, resolver = Eio.Promise.create () in
  {
    promise;
    thunk =
      Eio.Lazy.from_fun ~cancel:`Record (fun () ->
          let result = try Result.ok @@ f () with e -> Result.error e in
          let _ = Eio.Promise.try_resolve resolver () in
          match result with Ok a -> a | Error e -> raise e);
  }

let force { thunk; _ } = Eio.Lazy.force thunk
