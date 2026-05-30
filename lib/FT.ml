include BatFingerTree

let rec mattch (compare : 'a -> 'a -> bool) (ft : 'a t) (to_compare : 'a list) =
  match (front ft, to_compare) with
  | None, [] -> true
  | None, _ -> false
  | Some _, [] -> false
  | Some (remaining_ft, to_match_ft), to_match_list :: remaining_list ->
      compare to_match_ft to_match_list
      && mattch compare remaining_ft remaining_list

let filter_map (f : 'a -> 'b option) =
  fold_left
    (fun acc elem ->
      match f elem with None -> acc | Some result -> snoc acc result)
    empty

let filter f = filter_map (fun elem -> if f elem then Some elem else None)

let concat_map (f : 'a -> 'b t) =
  fold_left (fun acc elem -> append acc @@ f elem) empty

let of_option = function Some v -> singleton v | None -> empty

let rec sort (compare : 'a -> 'a -> int) (input : 'a t) =
  let merge cmp l1 l2 =
    let rec go acc l1 l2 =
      match (front l1, front l2) with
      | None, _ -> append acc l2
      | _, None -> append acc l1
      | Some (t1, h1), Some (t2, h2) ->
          if cmp h1 h2 <= 0 then go (snoc acc h1) t1 l2
          else go (snoc acc h1) l1 t2
    in
    go empty l1 l2
  in
  match size input with
  | 0 | 1 -> input
  | n ->
      let left, right = split_at input (n / 2) in
      merge compare (sort compare left) (sort compare right)

let group (cmp : 'a -> 'a -> int) (ft : 'a t) : 'a t t =
  let sorted = sort cmp ft in
  let fold first rest =
    fold_left
      (fun (acc, agr, last) elem ->
        if cmp last elem = 0 then (acc, snoc agr elem, elem)
        else (snoc acc agr, singleton elem, elem))
      (empty, singleton first, first)
      rest
  in
  match front sorted with
  | None -> empty
  | Some (tl, hd) ->
      let groups, lastgr, _ = fold hd tl in
      snoc groups lastgr

let rec find_opt (f : 'a -> bool) (ft : 'a t) : 'a option =
  Option.bind (front ft) (fun (remaining, first) ->
      if f first then Some first else find_opt f remaining)
