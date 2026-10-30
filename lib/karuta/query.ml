include Types

let compile (query : Ast.Module.query_ref Location.with_location option)
    (compiler : state Shared.Compiler.t) =
  match query with
  | None -> compiler
  | Some { loc; content = { name; args } } ->
      let arity = FT.size args in
      (* TODO: undo the hack we did for the WAM with the fake declaration *)
      let open Beam in
      let declaration =
        let fun_args =
          (* TODO: Builder does not use FT yet, hence we keep the list *)
          List.init (arity + 1) @@ Fun.const Builder.pattern_wildcard
        in
        let args = FT.to_list args in
        args |> List.map Builder.var
        |> Builder.call (Builder.atom "")
        |> List.fold_right Ukanren.query_variable args
        |> List.fold_right Declaration.call_with_fresh args
        |> Builder.single_function_declaration name fun_args
      in
      let module_ = Builder.atom (Module_name.of_filepath compiler.filename) in
      let rnt_query =
        Builder.call (Builder.atom name)
          (List.init (arity + 1) @@ Fun.const (Builder.atom ""))
        |> fun goal ->
        Builder.call_with_module (Builder.atom "rnt")
          (Builder.atom "relation") [ module_; goal ]
        |> Builder.single_function_declaration "rnt_query" []
      in
      (* What RNT runs for this module's image: the query as a relation
         RNT can page through. *)
      let rnt_init =
        Builder.call_with_module (Builder.atom "rnt") (Builder.atom "image")
          [ module_; Builder.list_expr (List.map Builder.atom (FT.to_list args)) ]
        |> Builder.single_function_declaration "rnt_init" []
      in
      (* Run by RNT when it boots the node: registers rnt_query/0, with
         this module's .beam, as \karuta\program\<module>. *)
      let export =
        Beam.Builder.Attribute.export
          [ (name, arity + 1); ("rnt_query", 0); ("rnt_init", 0) ]
      in
      {
        compiler with
        output =
          FT.cons
            (FT.append compiler.output
               (FT.of_list [ declaration; rnt_query; rnt_init ]))
            export;
        env =
          {
            compiler.env with
            query = Some (Location.add_loc { Ast.name; arity } loc);
          };
      }
