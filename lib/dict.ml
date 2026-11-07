module type CONFIG = sig
  type key
  type value

  val empty_value : value
  val key_value_flush : key -> value -> unit
end

module Make (Config : CONFIG) = struct
  type key = Config.key
  type value = Config.value

  let current_key : Config.key Eio.Fiber.key = Eio.Fiber.create_key ()

  let with_key : 'a. Config.key -> (unit -> 'a) -> 'a =
   fun key -> Eio.Fiber.with_binding current_key key

  let rec swap a f =
    let v = Atomic.get a in
    if Atomic.compare_and_set a v (f v) then () else swap a f

  let key_by_value : (Config.key, Config.value) BatMap.t Atomic.t =
    Atomic.make BatMap.empty

  let update update_value =
    Eio.Fiber.get current_key |> Option.get |> fun key ->
    swap key_by_value (BatMap.modify_def Config.empty_value key update_value)

  let flush_iter () =
    key_by_value |> Atomic.get |> BatMap.iter Config.key_value_flush

  let flush_one_shot f = key_by_value |> Atomic.get |> f
end
