%token <string> LITERAL_ATOM
%token <string> IDENT
%token <string> INTEGER
%token <string> UPPER_IDENT
%token LEFT_DELIM
%token RIGHT_DELIM
%token DIRECTIVE_LEFT_DELIM
%token DIRECTIVE_RIGHT_DELIM
%token PIPE
%token COMMA
%token DOT
%token HOLDS
%token MODULE_SEPARATOR
%token EOF
%token QUERY
%token EXPRESSION_COMMENT

%start <Ast.ParserClause.t list> file
%%

program(terminator):
  | declaration program(terminator)
    { ($1 :: $2) }
  | EXPRESSION_COMMENT declaration program(terminator)
    { $3 }
  | query program(terminator)
    { ($1 :: $2) }
  | directive program(terminator)
    { ($1 :: $2) }
  | EXPRESSION_COMMENT query program(terminator)
    { $3 }
  | terminator
    { [] }
  ;

file:
  | program(EOF)
    { $1 }
  ;

located(X):
  | x=X
    {Location.add $startpos $endpos x}
  ;

functorr:
  | functor_name = functor_label; LEFT_DELIM; identifiers = list_identifiers
  { ({ name = functor_name; elements = identifiers; arity = List.length identifiers } : Ast.Expr.func) }
  | functor_name = functor_label
  { ({ name = functor_name; elements = []; arity = 0 } : Ast.Expr.func) }
  ;

functor_label:
  | module_name = located(IDENT); MODULE_SEPARATOR; remaining = functor_label;
    { let (prefix, name) = remaining in (module_name :: prefix, name) }
  | name = located(IDENT);
    { [], name }
  | name = located(LITERAL_ATOM);
    { [], name }
  ;

maybe_functor:
  | EXPRESSION_COMMENT; functorr;
  { None }
  | located(functorr)
  { Some $1 }
  ;

declaration_:
  | functor_elem = functorr; DOT
    { Ast.ParserClause.Declaration {head = functor_elem; body = []}}
  | functor_elem = functorr; HOLDS; statements = separated_nonempty_list(COMMA, maybe_functor); DOT
    { Ast.ParserClause.Declaration { head = functor_elem; body = statements |> List.concat_map Option.to_list } }
  ;

declaration: located(declaration_) {$1}

query_:
  | queries = separated_nonempty_list(COMMA, located(functorr)); QUERY
    { Ast.ParserClause.QueryConjunction queries }
  ;

query: located(query_) {$1}

program_fragment:
  | program(DIRECTIVE_RIGHT_DELIM)
    { $1 }
  ;

directive_:
  (* TODO: add location to the functorr here *)
  | HOLDS; functor_elem = located(functorr); DOT
    { Ast.ParserClause.Directive (functor_elem, []) }
  | HOLDS; functor_elem = located(functorr); DIRECTIVE_LEFT_DELIM; body = program_fragment; DOT
    { Ast.ParserClause.Directive (functor_elem, body) }
  ;

directive: located(directive_) {$1}

list_identifiers:
  | RIGHT_DELIM { [] }
  | expression COMMA list_identifiers { $1 :: $3 }
  | EXPRESSION_COMMENT expression COMMA list_identifiers { $4 }
  | expression RIGHT_DELIM { [$1] }
  | EXPRESSION_COMMENT expression RIGHT_DELIM { [] }
  ;

expression_:
  | INTEGER { Ast.Expr.Integer (int_of_string $1) }
  | UPPER_IDENT { Ast.Expr.Variable $1 }
  | functor_elem = functorr { Ast.Expr.Functor functor_elem }
  | LEFT_DELIM; expressions = separated_nonempty_list(COMMA, expression); PIPE; tail = expression; RIGHT_DELIM
    { let open Location in
      (tail.content, tail.loc)
      |> List.fold_right (fun element (acc, location) ->
                           (Ast.Expr.Cons (element, {content = acc; loc = location}),
                            {startl = element.loc.startl; endl = tail.loc.endl}))
                         expressions
      |> (fun (x, _) -> x)
    }
  | LEFT_DELIM; expressions = list_identifiers
    { let open Location in
      (Ast.Expr.Nil, {startl = Location.to_t $endpos; endl = Location.to_t $endpos})
      |> List.fold_right (fun element (acc, loc) ->
                        (Ast.Expr.Cons (element, {content = acc; loc}),
                         {startl = element.loc.startl; endl = loc.endl}))
                         expressions
      |> (fun (x, _) -> x)}

expression: located(expression_) {$1}
