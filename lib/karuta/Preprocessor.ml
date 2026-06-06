open Shared.Preprocessor

let rename_arg ({ loc; _ } as expr : Ast.Expr.t) (counter : int) :
    Ast.Expr.func Location.with_location =
  let open Ast in
  let new_var = Location.add_loc (Expr.Variable (string_of_int counter)) loc in
  {
    content =
      {
        name =
          ( FT.singleton { Location.content = "karuta"; loc },
            { content = "eq"; loc } );
        elements = FT.of_list [ new_var; expr ];
      };
    loc;
  }

let rename_declaration
    ({ head = { elements; _ }; body } : Ast.ParserClause.decl) : Ast.Clause.decl
    =
  let arity = FT.size elements in
  let _, new_body =
    FT.fold_right
      (fun (counter, body') arg ->
        let to_append = rename_arg arg counter in
        (counter - 1, FT.cons body' to_append))
      (arity - 1, body)
      elements
  in
  { body = new_body; original_arg_list = elements }

let from_declaration (clause : Ast.ParserClause.t) :
    Ast.Clause.decl Location.with_location =
  match clause with
  | { content = Declaration decl; loc } ->
      { content = rename_declaration decl; loc }
  | _ ->
      Logger.simply_unreachable "unreachable from_declaration";
      exit 1

let rec parser_to_compiler :
    t -> Ast.ParserClause.t -> (Types.directives, Types.mods) output =
 fun ({ dependencies; filename } as preprocessor) clause ->
  let open Location in
  let open Ast in
  match clause with
  | { content = Directive (head, body); _ } ->
      let grouped_body = FT.map (group_clauses preprocessor) body in
      let dependencies, _grouped_clauses =
        FT.fold_right
          (fun (deps_acc, clauses_acc) { dependencies; clauses } ->
            ( Shared.DependencyGraph.merge dependencies deps_acc,
              FT.cons clauses_acc clauses ))
          (dependencies, FT.empty) grouped_body
      in
      let dependencies =
        if Ast.Expr.match_func head.content [ "import" ] then
          if FT.is_empty body then (
            match FT.front head.content.elements with
            | Some (rest, singleton) when FT.is_empty rest ->
                let external_dep =
                  Ast.Expr.extract_unqualified_atom singleton
                in
                DependencyGraph.add filename external_dep dependencies
            | None ->
                Logger.error head.loc
                  "Directive 'import' cannot be an empty functor";
                exit 1
            | _ ->
                Logger.error head.loc
                  "Directive 'import' cannot have multiple expressions within";
                exit 1)
          else (
            Logger.error head.loc "Directive 'import' cannot have a body";
            exit 1)
        else dependencies
      in
      { dependencies; clauses = FT.empty }
      (* { *)
      (*   dependencies; *)
      (*   clauses = *)
      (*     FT.singleton *)
      (*       { *)
      (*         content = *)
      (*           Ast.Clause.Directive *)
      (*             (target_specific_directive head grouped_clauses loc); *)
      (*         loc; *)
      (*       }; *)
      (* } *)
  | { content = Declaration decl; loc } ->
      {
        dependencies;
        clauses =
          FT.singleton
            {
              content =
                Ast.Clause.MultiDeclaration
                  (decl_header decl, rename_declaration decl, FT.empty);
              loc;
            };
      }
  | { content = QueryConjunction calls; loc } ->
      let folder set call =
        S.union set (find_variables @@ Expr.Functor (strip_loc call))
      in
      let variables = FT.fold_left folder S.empty calls in
      let list_variables = FT.of_list @@ S.to_list variables in
      let query_name = "" in
      let head : Expr.func =
        {
          name = (FT.empty, { content = query_name; loc });
          elements =
            FT.map
              (fun var -> { content = Expr.Variable var; loc })
              list_variables;
        }
      in
      let fake_decl : ParserClause.decl = { head; body = calls } in
      let declaration =
        {
          content =
            Ast.Clause.MultiDeclaration
              (decl_header fake_decl, rename_declaration fake_decl, FT.empty);
          loc;
        }
      in
      let query =
        {
          content =
            Ast.Clause.Query { name = query_name; args = list_variables };
          loc;
        }
      in
      { dependencies; clauses = FT.of_list [ declaration; query ] }

and group_clauses ({ dependencies; _ } as preprocessor : t)
    (clauses : Ast.ParserClause.t FT.t) =
  let multi_mapper (group : Ast.ParserClause.t FT.t) =
    match FT.front group with
    | Some (remaining, single) when remaining = FT.empty ->
        parser_to_compiler preprocessor single
    | Some (many, { content = Declaration first; loc }) ->
        {
          dependencies;
          clauses =
            FT.singleton
              {
                Location.content =
                  Ast.Clause.MultiDeclaration
                    ( decl_header first,
                      rename_declaration first,
                      FT.map from_declaration many );
                loc;
              };
        }
    | _ ->
        Logger.simply_unreachable "unreachable group";
        exit 1
  in
  clauses
  |> FT.filter_map remove_comments
  |> FT.map check_empty_heads |> FT.group compare_clauses
  |> fold_map preprocessor.dependencies multi_mapper

let preprocess_clauses (_preprocessor : t) (_clauses : Ast.ParserClause.t FT.t)
    : (Types.directives, Types.mods) output =
  failwith "Karuta"
