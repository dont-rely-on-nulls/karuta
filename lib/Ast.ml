module type EXPR = sig
  type t
  type base
  type func
  type func_label

  val extract_variable : t -> string
  val extract_unqualified_atom : t -> string
  val is_functor : t -> bool
end

type head = { name : string; arity : int } [@@deriving show, ord]

module ModuleF (Expr : EXPR) = struct
  type declaration = head * decl Location.with_location

  and multi_declaration =
    head * (decl Location.with_location * decl Location.with_location FT.t)

  and multi_declaration_env =
    ( head,
      decl Location.with_location * decl Location.with_location FT.t )
    BatMap.t

  and declaration_env = (head, decl Location.with_location) BatMap.t
  and query_ref = { name : string; args : string FT.t }

  and ('directives, 'mods) signature_body = {
    declarations : declaration_env;
    directives : ('directives, 'mods) directive Location.with_location FT.t;
  }

  and ('directives, 'mods) signature_ref =
    | Named of Expr.func_label
    | Inlined of ('directives, 'mods) signature_body

  and ('directives, 'mods) directive =
    | Module of ('directives, 'mods) module_body
    | Signature of {
        name : string Location.with_location;
        body : ('directives, 'mods) signature_body;
      }
    | TargetSpecific of 'directives

  and ('directives, 'mods) module_body = {
    name : string Location.with_location;
    signature :
      ('directives, 'mods) signature_ref Location.with_location option;
    declarations : multi_declaration_env;
    directives : ('directives, 'mods) directive Location.with_location FT.t;
    target_specific : 'mods;
    query : query_ref Location.with_location option;
  }

  and decl = {
    body : Expr.func Location.with_location FT.t;
    original_arg_list : Expr.t FT.t;
  }

  and ('directives, 'mods) t =
    ('directives, 'mods) module_body Location.with_location

  let signature_populated (s : ('directives, 'mods) signature_body) : bool =
    not (FT.is_empty s.directives && BatMap.is_empty s.declarations)

  let multi_declaration_env_to_declaration_env
      (multi_declaration_env : multi_declaration_env) : declaration_env =
    BatMap.map Pair.fst multi_declaration_env
end

module Expr = struct
  type func_label =
    string Location.with_location FT.t * string Location.with_location

  let compare_func_label ((lhs_qualifier, lhs_name) : func_label)
      ((rhs_qualifier, rhs_name) : func_label) : int =
    let compare_segment =
     fun (lhs : string Location.with_location)
         (rhs : string Location.with_location) ->
      String.compare lhs.content rhs.content
    in
    match FT.compare compare_segment lhs_qualifier rhs_qualifier with
    | 0 -> compare_segment lhs_name rhs_name
    | n -> n

  type func = { name : func_label; elements : t FT.t }

  and base =
    | Variable of string
    | Functor of func
    | Integer of int
    | Nil
    | Cons of t * t

  and t = base Location.with_location

  let on f2 f1 v1 v2 = f2 (f1 v1) (f1 v2)

  let compare_func lhs rhs : int =
    match on Int.compare FT.size lhs.elements rhs.elements with
    | 0 -> compare_func_label lhs.name rhs.name
    | n -> n

  let atom : func_label -> func = fun name -> { name; elements = FT.empty }

  let func : func_label -> t FT.t -> func =
   fun name elements -> { name; elements }

  let func_label_of_string : string Location.with_location -> func_label =
   fun s -> (FT.empty, s)

  let functorr f = Functor f
  let integer i = Integer i
  let variable v = Variable v

  let extract_variable : t -> string = function
    | { content = Variable name; _ } -> name
    | _ ->
        Logger.simply_error "Trying to extract a variable wrongly";
        exit 1

  let extract_qualified_func_label :
      func -> string Location.with_location * string Location.with_location =
   fun func ->
    let qualifiers, name = func.name in
    match FT.front qualifiers with
    | None ->
        Logger.error name.loc "Expected functor to have a qualified label";
        exit 1
    | Some (remaining, qualifier) when FT.is_empty remaining -> (qualifier, name)
    | Some (_, qualifier) ->
        Logger.error qualifier.loc "Expected functor to have a single qualifier";
        exit 1

  let func_label : func -> string = function
    | { name = _, name; _ } -> name.content

  let match_func : func -> string list -> bool =
   fun func to_compare ->
    let names, name = func.name in
    FT.mattch ( = )
      (FT.snoc (FT.map Location.strip_loc names) name.content)
      to_compare

  let extract_func_label : func -> string =
   fun func ->
    let qualifiers, name = func.name in
    match FT.front qualifiers with
    | None -> name.content
    | Some (_, first_segment) ->
        Logger.error first_segment.loc
          "Expected functor to have an unqualified label";
        exit 1

  let get_functor_label : t -> func_label = function
    | { content = Functor f; _ } -> f.name
    | _ ->
        Logger.simply_error "Trying to get a functor label out of a non-functor";
        exit 1

  let extract_functor_label : t -> string = function
    | { content = Functor f; _ } -> extract_func_label f
    | _ ->
        Logger.simply_error
          "Trying to extract a functor label out of a non-functor";
        exit 1

  let extract_unqualified_atom : t -> string =
   fun { content; loc } ->
    match content with
    | Functor func -> (
        let qualifiers, unqualified_name = func.name in
        match FT.front qualifiers with
        | None when FT.is_empty func.elements -> unqualified_name.content
        | None ->
            Logger.error loc "Expected unqualified atom and it has arguments";
            exit 1
        | Some _ ->
            Logger.error loc "Expected unqualified atom and it is qualified";
            exit 1)
    | _ ->
        Logger.error loc "Expected unqualified atom";
        exit 1

  let first_functor_atom_arg : t -> string =
   fun { content; loc } ->
    match content with
    | Functor func -> (
        match FT.front func.elements with
        | None ->
            Logger.error loc
              "Tried to extract first functor atom argument from an atom";
            exit 1
        | Some (_, first) -> extract_unqualified_atom first)
    | _ ->
        Logger.error loc
          "Tried to extract first functor atom argument from a non Functor";
        exit 1

  let is_functor : t -> bool = function
    | { content = Functor _; _ } -> true
    | _ -> false
end

module ParserClauseF (Expr : EXPR) = struct
  type base =
    | Declaration of decl
    | QueryConjunction of Expr.func Location.with_location FT.t
    | Directive of directive

  and directive =
    Expr.func Location.with_location * t FT.t Location.with_location FT.t

  and t = base Location.with_location
  and decl = { head : Expr.func; body : Expr.func Location.with_location FT.t }

  let query q = QueryConjunction q
  let declaration d = Declaration d
  let directive (header, bodies) = Directive (header, bodies)

  let is_decl : t -> bool = function
    | { content = Declaration _; _ } -> true
    | _ -> false
end

module Module = ModuleF (Expr)
module ParserClause = ParserClauseF (Expr)
