type t = BatSet.String.t BatMap.String.t

let empty = BatMap.String.empty

let merge =
  BatMap.String.merge (fun _ l r ->
      match (l, r) with
      | None, None -> None
      | v, None | None, v -> v
      | Some lset, Some rset -> Some (BatSet.String.union lset rset))

let add key value graph =
  let open BatMap.String in
  match find_opt key graph with
  | None -> add key (BatSet.String.singleton value) graph
  | Some set -> add key (BatSet.String.add value set) graph

let invert graph =
  let invert_one node children acc =
    BatSet.String.fold (fun child acc -> add child node acc) children acc
  in
  BatMap.String.fold invert_one graph BatMap.String.empty

type expansion_state = {
  forward : t;
  backward : t;
  visited : BatSet.String.t;
  next : string BatFingerTree.t;
}

type expansion_result = expansion_state Error.attempt

(** Returns true if l depends on r in the given fully expanded graph, false
    otherwise *)
let depends expanded_graph l r =
  match BatMap.String.find_opt l expanded_graph with
  | None -> false
  | Some deps -> BatSet.String.mem (ModuleName.of_filepath r) deps

let no_cycles graph =
  (* TODO: explicitly say which files participate in each cycle instead
       of just saying each file depends on itself. *)
  let module Map = BatMap.String in
  if
    BatEnum.fold
      (fun has_cycle k ->
        if depends graph k k then (
          Logger.simply_error @@ "File " ^ k ^ " depends on itself";
          true)
        else has_cycle)
      false
    @@ Map.keys graph
  then exit 1;
  graph

let expand graph : t Error.attempt =
  let module Set = BatSet.String in
  let module Map = BatMap.String in
  let atom_to_filepath =
    Map.of_enum
    @@ BatEnum.map (fun k -> (ModuleName.of_filepath k, k))
    @@ Map.keys graph
  in
  let bridge_ancestor relatives node graph =
    if Set.is_empty relatives then graph
    else Map.modify_def Set.empty node (Set.union relatives) graph
  in
  let rec go (acc : expansion_result) : expansion_result =
    let open Error in
    let* { visited; forward; next; backward } = acc in
    match FT.front next with
    | Some (more, current) when Set.mem current visited ->
        go @@ ok { visited; forward; next = more; backward }
    | Some (more, current) -> (
        let visited = Set.add current visited in
        match
          Map.find_opt
            (Map.find_default current current atom_to_filepath)
            forward
        with
        | None -> go @@ ok { visited; forward; next = more; backward }
        | Some children ->
            let parents =
              Option.value ~default:Set.empty @@ Map.find_opt current backward
            in
            let forward = Set.fold (bridge_ancestor children) parents forward in
            let backward =
              Set.fold (bridge_ancestor parents) children backward
            in
            go
            @@ ok
                 {
                   visited;
                   forward;
                   next =
                     children |> Set.enum
                     |> BatEnum.filter (fun c -> not @@ Set.mem c visited)
                     |> BatEnum.fold FT.snoc more;
                   backward;
                 })
    | None -> Ok { visited; forward; next; backward }
  in
  Error.map (fun { forward; _ } -> no_cycles forward)
  @@ go
  @@ Error.ok
       {
         visited = BatSet.String.empty;
         forward = graph;
         backward = invert graph;
         next = BatFingerTree.of_enum (BatMap.String.keys graph);
       }

let sort (expanded_graph : t) (files : string FT.t) =
  let compare_files l r =
    if depends expanded_graph l r then 1
    else if depends expanded_graph r l then -1
    else 0
  in
  FT.sort compare_files files
