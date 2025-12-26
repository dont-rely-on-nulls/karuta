open Error
module Form = Beam.Core.Form (Beam.Core.Erlang)

let parse : string -> Ast.parser_clause list attempt = function
  | "" -> error Error.EmptyFilepath
  | str ->
      In_channel.with_open_text str @@ fun inc ->
      if in_channel_length inc = 0 then error @@ Error.EmptyFile str
      else ok @@ Parse.parse str

let preprocess (filepath : string) :
    Ast.parser_clause list -> Ast.clause list attempt = function
  | [] -> error @@ Error.CouldNotPreprocess filepath
  | decls_queries -> ok @@ fst @@ Preprocessor.group_clauses decls_queries

let compile' (compiler : Compiler.t) :
    Ast.parser_clause list -> Form.t BatFingerTree.t = function
  | [] ->
      Logger.simply_unreachable
        "Compiler error: unreachable when executing compile function.";
      exit 1
  | decls_queries ->
      let grouped_clauses, defined_symbols =
        Preprocessor.group_clauses decls_queries
      in
      Compiler.compile (grouped_clauses, { compiler with defined_symbols })

(* let compile filename = compile' (Compiler.initialize filename) *)

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

let compile (filepath : string) : Ast.clause list attempt =
  filepath |> parse ||> preprocess filepath

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
(*   match (Parse.parse content, compiler_and_computer) with *)
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
