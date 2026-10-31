open Error
module Form = Beam.Core.Form (Beam.Core.Erlang)

(* let eval ((compiler, computer) : Compiler.t * Machine.t) : *)
(*     (Compiler.t * Machine.t) option = *)
(*   match compiler.entry_point with *)
(*   | None -> None *)
(*   | Some entry_point -> *)
(*       let computer = *)
(*         Evaluator.eval compiler.functor_table *)
(*           { computer with p_register = entry_point.p_register } *)
(*       in *)
(*       Some (compiler, computer) *)

(* TODO: compilation cache *)
let compile ({ sakura; artifact } : Shared.Compiler.Options.t)
    (persist : Shared.Compiler.Persist.both) (filepaths : string list) :
    unit attempt =
  let sakura_files, karuta_files =
    BatSet.String.partition Sakura.Preprocessor.is_sakura_file
    @@ BatSet.String.of_list filepaths
  in
  let module Lookup = Karuta.Lookup in
  let module Sakura : Shared.Compiler.COMPILER =
    Shared.Compiler.Make (Sakura.Module)
  in
  let module Karuta : Shared.Compiler.COMPILER =
    Shared.Compiler.Make (Karuta.Module)
  in
  let sakura_modules : Shared.Compiler.externals =
    if BatSet.String.is_empty sakura_files then BatMap.String.empty
    else
      match sakura with
      | None ->
          Logger.simply_error
            "Tried to compile Sakura files without a configuration";
          exit 1
      | Some { root_module; _ } ->
          let create_builtin modules =
            Shared.Compiler.builtin_module ~name:root_module ~predicates:[]
              ~modules
          in
          let eager_create_module =
            let create_module = function
              | None -> exit 1
              | Some v -> Shared.Compiler.Module v
            in
            Location.fmap @@ Fun.compose create_module Lazy.force
          in
          (try Sakura.compile_files persist sakura_files BatMap.String.empty
           with Exit -> exit 1)
          |> BatMap.String.map eager_create_module
          |> create_builtin
          |> Location.fmap (Fun.compose Lazy.from_val Option.some)
          |> BatMap.String.singleton root_module
  in
  let compiled_modules =
    try Karuta.compile_files persist karuta_files sakura_modules
    with Exit -> exit 1
  in
  match artifact with
  | Library -> Ok ()
  | Executable { root_module; filename } -> (
      let module Map = BatMap.String in
      match Map.find_opt root_module compiled_modules with
      | None ->
          Logger.simply_error
            "Root module for the executable could not be found";
          exit 1
      | Some { content; loc } -> (
          match
            Lookup.check_dependency_cycle loc.filename @@ fun () ->
            Lazy.force content
          with
          (* TODO: allow the use of queries from nested modules *)
          | Some { query = None; _ } ->
              Logger.error loc "Module does not have a query";
              exit 1
          | Some { query = Some query; _ } ->
              Ok
                (Executable.emit
                   {
                     persist = persist.executable;
                     query = query.content;
                     sakura =
                       (if BatSet.String.is_empty sakura_files then None
                        else sakura);
                     filename;
                     root_module;
                   })
          | None -> exit 1))

let run ?(limit : int option = None)
    (executable : Shared.Compiler.Options.executable)
    (persist : Shared.Compiler.Persist.both) (filepaths : string list) :
    string attempt =
  let open Shared.Compiler.Options in
  let open Error in
  compile
    (initialize ?sakura:None ~artifact:(Executable executable) ())
    persist filepaths
  ||> fun () ->
  match
    Posix.run_process
      (match limit with
      | None -> [| executable.filename |]
      | Some n -> [| executable.filename; string_of_int n |])
  with
  | Ok _ as ok -> ok
  | Error status ->
      Error (FailedToRunExternalProgram (executable.filename, status))

(* let load' filter_fn (filepath : string) : Compiler.t * Machine.t = *)
(*   filepath |> parse |> List.filter filter_fn |> compile *)

(* let load = load' (Fun.const true) *)
(* let load_decls = load' Ast.ParserClause.is_decl *)

(* let load_many_decls : string list -> (Compiler.t * Machine.t) option = function *)
(*   | [] -> None *)
(*   | f :: fs -> *)
(*       let get_decls filepath = *)
(*         filepath |> parse *)
(*         |> List.filter Ast.ParserClause.is_decl *)
(*         |> BatFingerTree.of_list *)
(*       in *)
(*       fs *)
(*       |> List.fold_left *)
(*            (fun acc f' -> BatFingerTree.append acc @@ get_decls f') *)
(*            (get_decls f) *)
(*       |> BatFingerTree.to_list |> compile |> Option.some *)

(* let continue content compiler_and_computer : (Compiler.t * Machine.t) option = *)
(*   match (Parser.parse content, compiler_and_computer) with *)
(*   | [], _ -> *)
(*       print_endline ("Parser error. Incorrect definition: " ^ content); *)
(*       None *)
(*   | decls_queries, Some (compiler, computer) -> *)
(*       decls_queries *)
(*       |> compile' *)
(*            ( { compiler with entry_point = None }, *)
(*              { computer with query_variables = BatMap.empty } ) *)
(*       |> eval *)
(*   | decls_queries, None -> decls_queries |> compile |> eval *)
