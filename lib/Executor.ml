open Error
module Form = Beam.Core.Form (Beam.Core.Erlang)

let parse : string -> Ast.ParserClause.t list attempt = function
  | "" -> error Error.EmptyFilepath
  | str ->
      In_channel.with_open_text str @@ fun inc ->
      if in_channel_length inc = 0 then error @@ Error.EmptyFile str
      else ok @@ Parser.parse str (In_channel.input_all inc)

let preprocess (preprocessor : Preprocessor.t) :
    Ast.ParserClause.t list -> Preprocessor.output attempt = function
  | [] -> error @@ Error.CouldNotPreprocess preprocessor.filename
  | decls_queries -> ok @@ Preprocessor.group_clauses preprocessor decls_queries

let compile' (step : Ast.Clause.t list * Compiler.Types.t -> Compiler.Types.t)
    (compiler : Compiler.Types.t) :
    Ast.Clause.t list -> Compiler.Types.t attempt = function
  | [] ->
      Logger.simply_unreachable
        "Compiler error: unreachable when executing compile function.";
      exit 1
  | decls_queries -> step (decls_queries, compiler) |> ok

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

module Karuta = struct
  let compile_clause = Karuta.compile_clause

  module Lookup = Compiler.Lookup
end

module Sakura = struct
  let compile_clause = Sakura.compile_clause

  module Lookup = Compiler.Lookup
end

type preprocessed_result = Preprocessor.DependencyGraph.t * Ast.Clause.t BatFingerTree.t BatMap.String.t

(* FIXME: hook up dependency information and sort the file list before compiling *)
let compile (persist : Compiler.Types.Persist.t) (filepaths : string list) :
    unit attempt =
  let compile_one_file (preprocessed : preprocessed_result) filepath =
    let extension = Filename.extension filepath in
    let compiler_config =
      if extension = ".skr" then
        (module Sakura : Compiler.Types.COMPILER_CONFIG)
      else (module Karuta : Compiler.Types.COMPILER_CONFIG)
    in
    let module Target = Compiler.Types.Make ((val compiler_config)) in
    compile' Target.step (Target.initialize persist filepath) []
  in
  let preprocess_one
      (acc : preprocessed_result attempt) filepath =
    let open Error in
    let* (dependencies, preprocessed) = acc in
    let preprocessor =
      { (Preprocessor.initialize filepath) with dependencies }
    in
    let* { dependencies; clauses } =
      filepath |> parse ||> preprocess preprocessor
    in
    ok (dependencies, BatMap.String.add filepath clauses preprocessed)
  in
  let* preprocessed = List.fold_left preprocess_one (ok (Preprocessor.DependencyGraph.empty, BatMap.String.empty)) filepaths in
  let rec compile_all = function
    | [] -> Ok ()
    | f :: files ->
        Result.bind (compile_one_file preprocessed f) (fun _ -> compile_all files)
  in
  compile_all filepaths

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
