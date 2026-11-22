module Compiler = Shared.Compiler.Make (Karuta.Module)

(* FIXME: We can handle this just fine without the mutables, but that
   is a problem for the clowns of the future. *)
type t = { runtime : Runtime.t;
           scratch : string;
           mutable declarations : string;
           mutable cursor : int; }

type outcome =
  | Defined
  | Answers of Runtime.page
  | Rejected
  | Failed of string

let start ~(path : string) : t =
  let dir = Filename.temp_dir "karuta" "session" in
  { runtime = Runtime.spawn ~path;
    (* FIXME: This can go away later. It's easier to test it this way
       since we don't have in memory streams like .NET (I
       guess?). Mainly treating a file ehre because of positon aware
       messages for some protocols, like LSP. *)
    scratch = Filename.concat dir "scratch.krt";
    declarations = "";
    cursor = 0; }

let stop {runtime; _} = Runtime.stop runtime

let compile {scratch; _} (source : string) : (string list * Ast.head option) option =
  Out_channel.with_open_text scratch (Fun.flip output_string source);
  let forms = ref [] in
  let persist : Shared.Compiler.Persist.both =
    { beam = (fun _ module_ -> forms := FT.to_list module_ :: !forms);
      executable = (fun _ _ -> ()); }
  in
  let compiled =
    Compiler.compile_files persist
      (BatSet.String.singleton scratch)
      BatMap.String.empty
    |> BatMap.String.map (fun { Location.content; loc } ->
        Shared.Compiler.check_dependency_cycle loc.filename (fun () ->
            Lazy.force content))
  in Logger.flush ();
  let text forms =
    "[" ^ String.concat ",\n" (List.map Beam.Serializer.Attribute.to_string forms) ^ "]."
  in
  let query =
    BatMap.String.find_opt (Module_name.of_filepath scratch) compiled
    |> Option.join
    |> Fun.flip Option.bind (fun (m : Shared.Compiler.compiled_module) ->
        Option.map Location.strip_loc m.query)
  in if BatMap.String.exists (fun _ m -> Option.is_none m) compiled then None
     else Some (List.rev_map text !forms, query)

let load session (source : string) : (Ast.head option, string) result option =
  compile session source
  |> Option.map
     @@ fun (modules, query) ->
     List.fold_left
       (fun acc forms ->
         Result.bind acc (fun () -> Runtime.load session.runtime forms))
       (Ok ()) modules
     |> Result.map (Fun.const query)

let next session ~(limit : int) : outcome =
  match Runtime.next session.runtime ~cursor:session.cursor limit with
  | Ok page -> Answers page
  | Error e -> Failed e

let eval session ~(limit : int) (input : string) : outcome =
  let source = session.declarations ^ input ^ "\n" in
  match load session source with
  | None -> Rejected
  | Some (Error e) -> Failed e
  | Some (Ok None) ->
      session.declarations <- source;
      Defined
  | Some (Ok (Some { arity; _ })) ->
     begin
       session.cursor <- session.cursor + 1;
       let module_ = Module_name.of_filepath session.scratch in
       match Runtime.query session.runtime ~cursor:session.cursor ~arity module_ with
       | Ok () -> next session ~limit
       | Error e -> Failed e
      end
