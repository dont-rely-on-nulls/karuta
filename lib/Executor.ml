open Error
module Form = Beam.Core.Form (Beam.Core.Erlang)

let parse : string -> Ast.ParserClause.t list attempt = function
  | "" -> error Error.EmptyFilepath
  | str ->
      In_channel.with_open_text str @@ fun inc ->
      if in_channel_length inc = 0 then error @@ Error.EmptyFile str
      else ok @@ Parser.parse str (In_channel.input_all inc)

let compile' (step : Ast.Clause.t list * Compiler.Types.t -> Compiler.Types.t)
    (compiler : Compiler.Types.t) :
    Ast.Clause.t list -> Compiler.Types.t attempt =
 fun decls_queries -> step (decls_queries, compiler) |> ok

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

type preprocessed_files = Ast.Clause.t BatFingerTree.t BatMap.String.t
type preprocessed_result = Preprocessor.DependencyGraph.t * preprocessed_files

let preprocess filepath =
  Error.map @@ Preprocessor.run (Preprocessor.initialize filepath)

(* FIXME: hook up dependency information and sort the file list before compiling *)
(* TODO: compilation cache *)
let compile ({ sakura_module_name } : Compiler.Types.cli)
    (persist : Compiler.Types.Persist.t) (filepaths : string list) :
    unit attempt =
  let sakura_filename = sakura_module_name ^ ".skr" in
  let sakura_files, karuta_files =
    BatList.partition Preprocessor.is_sakura_file filepaths
  in
  let* sakura_output =
    sakura_files
    |> Error.fold (fun parsed filepath ->
        parse filepath |> Error.map (List.append parsed))
    |> preprocess sakura_filename
  in
  let compile_one_file (preprocessed : preprocessed_files) filepath externals =
    let compiler_config : (module Compiler.Types.COMPILER_CONFIG) =
      if Preprocessor.is_sakura_file filepath then (module Sakura)
      else (module Karuta)
    in
    let module Target = Compiler.Types.Make ((val compiler_config)) in
    match BatMap.String.find_opt filepath preprocessed with
    | None ->
        Logger.simply_unreachable "We hit a file that doesn't exist";
        exit 1
    | Some body ->
        compile' Target.step
          (Target.initialize { persist; filename = filepath; externals })
        @@ BatFingerTree.to_list body
  in
  let preprocess_one (acc : preprocessed_result attempt) filepath =
    let open Error in
    let* dependencies, preprocessed = acc in
    let preprocessor =
      { (Preprocessor.initialize filepath) with dependencies }
    in
    let* { dependencies; clauses } =
      filepath |> parse |> Error.map @@ Preprocessor.run preprocessor
    in
    ok (dependencies, BatMap.String.add filepath clauses preprocessed)
  in
  let* dependency_graph, preprocessed_files =
    List.fold_left preprocess_one
      (ok
         ( sakura_output.dependencies,
           BatMap.String.singleton sakura_filename sakura_output.clauses ))
      filepaths
  in
  let* expanded_graph = Preprocessor.DependencyGraph.expand dependency_graph in
  let sorted_file_paths =
    Preprocessor.DependencyGraph.sort expanded_graph karuta_files
  in
  let rec compile_all imports = function
    | [] -> Ok ()
    | f :: files ->
        let* result = compile_one_file preprocessed_files f imports in
        compile_all result.externals files
  in
  compile_all BatMap.String.empty sorted_file_paths

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
