include Types
open Shared.Preprocessor

let is_sakura_file filepath = Filename.extension filepath = ".skr"
let init_mods = Fun.id

let preprocess_declaration
    ({ loc; _ } : Ast.ParserClause.decl Location.with_location) _ =
  Logger.error loc "Declarations are only allowed inside Sakura directives";
  exit 1

let parser_to_internal (declaration : Ast.ParserClause.decl) :
    string * Ast.Module.decl =
  ( Ast.Expr.extract_func_label declaration.head,
    { body = declaration.body; original_arg_list = declaration.head.elements }
  )

let report_error_cases (directive_loc : Location.location)
    ((qualifiers, name) :
      string Location.with_location FT.t * string Location.with_location) : unit
    =
  match FT.front qualifiers with
  | None ->
      Logger.error directive_loc
        "Sakura directives should be qualified with a single 'sakura' atom"
  | Some (remaining, _) when FT.size remaining <> 0 ->
      Logger.error directive_loc
        "Sakura directives should be qualified with a single 'sakura' atom"
  | Some (_, qualifier) when qualifier.content <> "sakura" ->
      Logger.error qualifier.loc
        "Sakura directives should be qualified with a single 'sakura' atom"
  | Some (_, qualifier) ->
      Logger.error name.loc "Undefined Sakura directive";
      Logger.simply_error @@ "Available directives are: "
      ^ Types.formatted_supported_directives

let preprocess_directive :
    (Types.directives, Types.mods) recur ->
    Ast.ParserClause.directive ->
    (Types.directives, Types.mods) one_output =
 fun _recur (head, bodies) ->
  let preprocess_bodies
      (f : Ast.Module.multi_declaration_env -> Types.directives) =
    match FT.front bodies with
    | None ->
        Logger.error head.loc
          "Sakura-exclusive directives can only have one body";
        exit 1
    | Some (remaining, _) when FT.size remaining <> 0 ->
        Logger.error head.loc
          "Sakura-exclusive directives can only have one body";
        exit 1
    | Some (_, body) ->
        let open Location in
        f
        @@ FT.fold_left
             (fun acc { content; loc } ->
               match content with
               | Ast.ParserClause.QueryConjunction _ ->
                   Logger.error loc "Cannot have query inside Sakura directive";
                   exit 1
               | Ast.ParserClause.Directive _ ->
                   Logger.error loc "Sakura directives are not nestable";
                   exit 1
               | Declaration decl ->
                   let name, decl = parser_to_internal decl in
                   Shared.Preprocessor.group_declaration name
                     (Location.add_loc decl loc)
                     acc)
             BatMap.empty body.content
  in
  TargetSpecificDirective
    (if Ast.Expr.match_func head.content [ "sakura"; "persisted" ] then
       preprocess_bodies (fun env ->
           Persisted (Ast.Module.multi_declaration_env_to_declaration_env env))
     else if Ast.Expr.match_func head.content [ "sakura"; "ephemeral" ] then
       preprocess_bodies (fun env -> Ephemeral env)
     else if Ast.Expr.match_func head.content [ "sakura"; "constraint" ] then
       preprocess_bodies (fun env -> Constraint env)
     else (
       report_error_cases head.loc head.content.name;
       exit 1))

let preprocess_query (loc : Location.location)
    (_ : Ast.Expr.func Location.with_location FT.t)
    (_ : (Types.directives, Types.mods) Ast.Module.module_body) :
    (Types.directives, Types.mods) Ast.Module.module_body =
  Logger.error loc "Queries are not allowed in Sakura";
  exit 1
