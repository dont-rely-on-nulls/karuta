include Types

let compile (query : Ast.Module.query option)
    (compiler : state Shared.Compiler.t) =
  match query with
  | None -> compiler
  | Some ({ content = calls; _ } as body) ->
      let open Ast in
      let open Location in
      let open Shared.Preprocessor in
      let folder set call =
        S.union set (find_variables @@ Expr.Functor (strip_loc call))
      in
      let variables = FT.fold_left folder S.empty calls |> S.remove "_" in
      let open Beam in
      let query_fn =
        body
        |> Location.fmap (fun body ->
            { Ast.Module.original_arg_list = FT.empty; body })
        |> Declaration.compile_single_body compiler
        |> S.fold Ukanren.query_variable variables
        |> S.fold
             (fun var body -> Declaration.call_with_fresh var [ body ])
             variables
        |> List.singleton |> Builder.lambda []
        |> fun v -> Builder.Expr.Fun v
      in
      let module_ = Builder.atom (Module_name.of_filepath compiler.filename) in
      let rnt_query =
        Builder.call (Builder.atom "") [] |> fun goal ->
        Builder.call_with_module (Builder.atom "rnt") (Builder.atom "relation")
          [ module_; goal ]
        |> List.singleton |> Builder.lambda []
        |> fun v -> Builder.Expr.Fun v
      in
      (* What RNT runs for this module's image: the query as a relation
         RNT can page through. *)
      let rnt_init =
        Builder.call_with_module (Builder.atom "rnt") (Builder.atom "image")
          [
            module_;
            Builder.list_expr (List.map Builder.atom (S.to_list variables));
          ]
        |> List.singleton |> Builder.lambda []
        |> fun v -> Builder.Expr.Fun v
      in
      (* Run by RNT when it boots the node: registers rnt_query/0, with
         this module's .beam, as \karuta\program\<module>. *)
      let export = Beam.Builder.Attribute.export [ ("", 0) ] in
      {
        compiler with
        output =
          compiler.output
          |> Fun.flip FT.snoc
               (Builder.single_function_declaration "" []
                  [
                    Builder.eq "Query" query_fn;
                    Builder.map
                      (List.map Builder.atom
                         [ "query"; "rnt-query"; "rnt-init" ])
                      [ Builder.var "Query"; rnt_query; rnt_init ];
                  ])
          |> Fun.flip FT.cons export;
        env = { compiler.env with query = Some body.loc };
      }
