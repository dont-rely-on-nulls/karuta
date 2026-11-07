include Types
open Shared.Preprocessor

let initial_mods = Fun.id
let init_imports () = BatMap.String.singleton "karuta" Location.dummy

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
  let new_body =
    FT.fold_right
      (fun (counter, body') arg ->
        let to_append = rename_arg arg counter in
        (counter - 1, FT.cons body' to_append))
      (arity - 1, body)
      elements
    |> snd
    |> FT.map
       @@ Fun.compose
            (Fun.compose
               ( Location.fmap @@ function
                 | Ast.Expr.Functor f -> f
                 | _ ->
                     Logger.simply_unreachable
                       "This cannot be anything other than a functor";
                     raise Exit )
               (Ast.Expr.rename_vars
               @@
               let discards = ref 0 in
               function
               | "_" ->
                   let current_count = !discards in
                   (* The # character is forbidden by the grammar in variable
                      names, so we cannot collide with a name from the source
                      program.
                    *)
                   let new_name = "Discard#" ^ string_of_int current_count in
                   discards := current_count + 1;
                   new_name
               | name -> name))
            (Location.fmap (fun f -> Ast.Expr.Functor f))
  in
  { body = new_body; original_arg_list = elements }

let preprocess_declaration (decl : Ast.ParserClause.decl Location.with_location)
    (declarations : Ast.Module.multi_declaration_env) :
    Ast.Module.multi_declaration_env =
  let open Location in
  let renamed = { loc = decl.loc; content = renamer decl.content } in
  Shared.Preprocessor.group_declaration
    (Ast.Expr.extract_func_label decl.content.head)
    renamed declarations

let preprocess_directive :
    Types.directives recur ->
    Ast.ParserClause.directive ->
    Types.directives one_output =
 fun _recur (head, _bodies) ->
  Logger.error head.loc "Unknown Karuta directive";
  raise Exit

let preprocess_query (loc : Location.region)
    (calls : Ast.Expr.func Location.with_location FT.t)
    (module_ : Types.directives Ast.Module.module_body) :
    Types.directives Ast.Module.module_body =
  let open Ast in
  let open Location in
  let folder set call =
    S.union set (find_variables @@ Expr.Functor (strip_loc call))
  in
  let variables = FT.fold_left folder S.empty calls |> S.remove "_" in
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
      BatMap.add
        {
          Ast.name = Ast.Expr.extract_func_label head;
          arity = FT.size list_variables;
        }
        declaration_body module_.declarations;
  }
