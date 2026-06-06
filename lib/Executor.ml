open Error
module Form = Beam.Core.Form (Beam.Core.Erlang)

let parse : string -> Ast.ParserClause.t FT.t attempt = function
  | "" -> error Error.EmptyFilepath
  | str ->
      In_channel.with_open_text str @@ fun inc ->
      if in_channel_length inc = 0 then error @@ Error.EmptyFile str
      else ok @@ Parser.parse str (In_channel.input_all inc)

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

type ('directives, 'mods) preprocessed_files =
  ('directives, 'mods) Ast.Clause.t BatFingerTree.t BatMap.String.t

type ('directives, 'mods) preprocessed_result =
  Preprocessor.DependencyGraph.t * ('directives, 'mods) preprocessed_files

(* FIXME: hook up dependency information and sort the file list before compiling *)
(* TODO: compilation cache *)
let compile ({ sakura; artifact } : Compiler.Options.t)
    (persist : Compiler.Persist.both) (filepaths : string list) : unit attempt =
  let sakura_files, karuta_files =
    FT.partition Preprocessor.is_sakura_file @@ FT.of_list filepaths
  in
  let module Sakura : Compiler.COMPILER = Compiler.Make (Sakura.Clause) in
  let module Karuta : Compiler.COMPILER = Compiler.Make (Karuta.Clause) in
  let* initial_dependencies, sakura_modules =
    if FT.is_empty sakura_files then
      Error.ok @@ (Preprocessor.DependencyGraph.empty, BatMap.String.empty)
    else
      match sakura with
      | None ->
          Logger.simply_error
            "Tried to compile Sakura files without a configuration";
          exit 1
      | Some { root_module; _ } ->
          let preprocess filepath =
            Error.map @@ Sakura.preprocess (Preprocessor.initialize filepath)
          in
          let sakura_filename = root_module ^ ".skr" in
          sakura_files
          |> Error.fold (fun parsed filepath ->
              parse filepath |> Error.map (FT.append parsed))
          |> preprocess sakura_filename
          ||> fun { dependencies; clauses } ->
          Error.ok
            (dependencies, BatMap.String.singleton sakura_filename clauses)
  in
  let preprocess_one_karuta acc filepath =
    let open Error in
    let* dependencies, preprocessed = acc in
    let preprocessor =
      { (Preprocessor.initialize filepath) with dependencies }
    in
    let* { dependencies; clauses } =
      filepath |> parse |> Error.map @@ Karuta.preprocess preprocessor
    in
    ok (dependencies, BatMap.String.add filepath clauses preprocessed)
  in
  let* dependency_graph, preprocessed_files =
    FT.fold_left preprocess_one_karuta
      (ok (initial_dependencies, BatMap.String.empty))
      karuta_files
  in
  let* expanded_graph = Preprocessor.DependencyGraph.expand dependency_graph in
  let sorted_file_paths =
    Preprocessor.DependencyGraph.sort expanded_graph karuta_files
  in
  let compiled_modules =
    (match sakura_modules |> BatMap.String.keys |> BatList.of_enum with
      | [ sakura_path ] ->
          Sakura.compile_files persist sakura_modules BatMap.String.empty
          @@ FT.singleton sakura_path
      | [] -> BatMap.String.empty
      | _ :: _ ->
          Logger.simply_unreachable
            "We always work with a single logical Sakura file";
          exit 1)
    |> fun compiled ->
    Karuta.compile_files persist preprocessed_files compiled sorted_file_paths
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
      | Some { content = Signature _; loc } ->
          Logger.error loc
            "Expected a module as the entry point but found a signature";
          exit 1
      | Some { content = Module { query = None; _ }; loc } ->
          Logger.error loc "Module does not have a query";
          exit 1
      | Some { content = Module { query = Some query; _ }; _ } ->
          Ok
            (EntryPoint.emit
               {
                 persist = persist.executable;
                 query = query.content;
                 sakura;
                 filename;
                 root_module;
               }))

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
