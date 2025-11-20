%{
%}

%token <string> LITERAL_ATOM
%token <string> IDENT
%token <string> INTEGER
%token <string> UPPER_IDENT
%token LEFT_DELIM
%token RIGHT_DELIM
%token PIPE
%token COMMA
%token DOT
%token HOLDS
%token EOF
%token QUERY
%token EXPRESSION_COMMENT

%start <Ast.ParserClause.t list> program
%%

program:
  | declaration program
    { ($1 :: $2) }
  | EXPRESSION_COMMENT declaration program
    { $3 }
  | query program
    { ($1 :: $2) }
  | EXPRESSION_COMMENT query program
    { $3 }
  | EOF
    { [] }
  ;

located(X):
  | x=X
    {Ast.Location.add $startpos $endpos x}
  ; 

functorr:
  | functor_name = IDENT; LEFT_DELIM; identifiers = list_identifiers
  { ({ namef = functor_name; elements = identifiers; arity = List.length identifiers } : Ast.func) }
  | functor_name = LITERAL_ATOM; LEFT_DELIM; identifiers = list_identifiers
  { ({ namef = functor_name; elements = identifiers; arity = List.length identifiers } : Ast.func) }
  | functor_name = IDENT;
  { ({ namef = functor_name; elements = []; arity = 0 } : Ast.func) }
  | functor_name = LITERAL_ATOM
  { ({ namef = functor_name; elements = []; arity = 0 } : Ast.func) }
  ;

maybe_functorr:
  | EXPRESSION_COMMENT; functorr;
  { None }
  | located(functorr)
  { Some $1 }
  ;

declaration_:
  | functor_elem = functorr; DOT
    { Ast.Declaration {head = functor_elem; body = []}}
  | functor_elem = functorr; HOLDS; statements = separated_nonempty_list(COMMA, maybe_functorr); DOT
    { Ast.Declaration { head = functor_elem; body = statements |> List.concat_map Option.to_list } }
  ;

declaration: located(declaration_) {$1}

query_:
  | queries = separated_nonempty_list(COMMA, located(functorr)); QUERY
    { Ast.QueryConjunction queries }
  ;

query: located(query_) {$1}

list_identifiers:
  | RIGHT_DELIM { [] }
  | expression COMMA list_identifiers { $1 :: $3 }
  | EXPRESSION_COMMENT expression COMMA list_identifiers { $4 }
  | expression RIGHT_DELIM { [$1] }
  | EXPRESSION_COMMENT expression RIGHT_DELIM { [] }
  ;

expression_:
  | INTEGER { Ast.Integer (int_of_string $1) }
  | UPPER_IDENT { Ast.Variable {namev = $1} }
  | functor_elem = functorr { Ast.Functor functor_elem }
  | LEFT_DELIM; expressions = separated_nonempty_list(COMMA, expression); PIPE; tail = expression; RIGHT_DELIM
    { let open Ast.Location in
      (tail.content, tail.loc)
      |> List.fold_right (fun element (acc, location) ->
                           ((Ast.Functor { namef = ""; elements = [element;{content = acc; loc = location}]; arity = 2 }),
                            {startl = element.loc.startl; endl = tail.loc.endl}))
                         expressions
      |> (fun (x, _) -> x)
    }
  | LEFT_DELIM; expressions = list_identifiers
    { let open Ast.Location in
      ((Ast.Functor { namef = ""; elements = []; arity = 0 }),
       {startl = Ast.Location.to_t $endpos; endl = Ast.Location.to_t $endpos})
      |> List.fold_right (fun element (acc, loc) ->
                        ((Ast.Functor { namef = ""; elements = [element;{content = acc; loc}]; arity = 2 }),
                         {startl = element.loc.startl; endl = loc.endl}))
                         expressions
      |> (fun (x, _) -> x)}

expression: located(expression_) {$1}

