(** Config is required to create a mutable dicitionary.

    Any types can be used for key and value. *)
module type CONFIG = sig
  type key
  type value

  val empty_value : value
  val key_value_flush : key -> value -> unit
end

(** Functor to create a mutable dictionary.

    This implementation assumes low write contention. *)
module Make (Config : CONFIG) = struct
  type key = Config.key
  (** Key type comes from Config *)

  type value = Config.value
  (** Value type comes from Config *)

  (** [current_key] internal-only current key control. This provides a
      concurrency-safe way to control the "context" of which key using fibers.
      It is not supposed to be used throughout the compiler. *)
  let current_key : Config.key Eio.Fiber.key = Eio.Fiber.create_key ()

  (** [with_key key continuation] updates the key for the lifetime of the
      provided continuation.

      @param key Desired key value to be used in continuation.
      @param continuation Function to run with reset log level.
      @return Executes the continuation after the reset. *)
  let with_key : 'a. Config.key -> (unit -> 'a) -> 'a =
   fun key -> Eio.Fiber.with_binding current_key key

  (** [swap a f] updates [a] using [f] atomically.

      @param a Atomic value to be updated.
      @param f Update function.
      @return atom has been updated atomically. *)
  let rec swap a f =
    let v = Atomic.get a in
    if Atomic.compare_and_set a v (f v) then () else swap a f

  (** [key_by_value] atomic (mutable) dictionary. This is modeled internally by
      a BatMap.t. *)
  let key_by_value : (Config.key, Config.value) BatMap.t Atomic.t =
    Atomic.make BatMap.empty

  (** [update update_value] updates a value in the dictionary with
      [update_value] function, given that we are already in the "context" of the
      key.

      @param update_value Update value function.
      @return
        perform the effect of updating the mutable dictionary using the provided
        function. *)
  let update update_value =
    Eio.Fiber.get current_key |> Option.get |> fun key ->
    swap key_by_value (BatMap.modify_def Config.empty_value key update_value)

  (** [flush_iter ()] calls the key_value_flush (provided in Config) in all
      entries of the internal BatMap.t.

      @param unit
      @return flushed all the entries one by one using pair flush function. *)
  let flush_iter () =
    key_by_value |> Atomic.get |> BatMap.iter Config.key_value_flush

  (** [flush_one_shot f] allows a flush to happen in the internal BatMap.t in a
      single shot. This exposes the internal BatMap.t data structure.

      @param f Function to be called on the inner dictionary.
      @return
        flushed the BatMap.t in a single shot with the provided flush function.
  *)
  let flush_one_shot f = key_by_value |> Atomic.get |> f
end
