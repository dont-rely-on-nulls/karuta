include Types
open Shared.Preprocessor

let initial_mods = Fun.id
let init_mods () = { imports = BatMap.String.empty }

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

let renamer ({ head = { elements; _ }; body } : Ast.ParserClause.decl) :
    Ast.Module.decl =
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

let preprocess_directive :
    (Types.directives, Types.mods) recur ->
    Ast.ParserClause.directive ->
    (Types.directives, Types.mods) one_output =
 fun _recur (head, bodies) ->
  let open Location in
  if Ast.Expr.match_func head.content [ "import" ] then
    if FT.is_empty bodies then (
      match FT.front head.content.elements with
      | Some (rest, singleton) when FT.is_empty rest ->
          let external_dep = Ast.Expr.extract_unqualified_atom singleton in
          let dependencies = BatSet.String.singleton external_dep in
          Update
            {
              dependencies;
              action =
                (fun { imports } ->
                  { imports = BatMap.String.add external_dep head.loc imports });
            }
      | None ->
          Logger.error head.loc "Directive 'import' cannot be an empty functor";
          exit 1
      | _ ->
          Logger.error head.loc
            "Directive 'import' cannot have multiple expressions within";
          exit 1)
    else (
      Logger.error head.loc "Directive 'import' cannot have a body";
      exit 1)
  else (
    Logger.error head.loc "Unknown Karuta directive";
    exit 1)

let preprocess_query (loc : Location.location)
    (calls : Ast.Expr.func Location.with_location FT.t)
    (module_ : (Types.directives, Types.mods) Ast.Module.module_body) :
    (Types.directives, Types.mods) Ast.Module.module_body =
  let open Ast in
  let open Location in
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
        FT.map (fun var -> { content = Expr.Variable var; loc }) list_variables;
    }
  in
  let fake_decl : ParserClause.decl = { head; body = calls } in
  let declaration_body = ({ content = renamer fake_decl; loc }, FT.empty) in
  let query : Ast.Module.query_ref with_location =
    { content = { name = query_name; args = list_variables }; loc }
  in
  {
    module_ with
    query = Some query;
    declarations =
      BatMap.add (decl_header fake_decl) declaration_body module_.declarations;
  }
