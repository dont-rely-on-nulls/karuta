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
  | functorr
  { Some $1 }
  ;

declaration:
  | functor_elem = functorr; DOT
    { Ast.Declaration {head = functor_elem; body = []}}
  | functor_elem = functorr; HOLDS; statements = separated_nonempty_list(COMMA, maybe_functorr); DOT
    { Ast.Declaration { head = functor_elem; body = statements |> List.concat_map Option.to_list } }
  ;

query:
  | queries = separated_nonempty_list(COMMA, functorr); QUERY
    { Ast.QueryConjunction queries }
  ;

list_identifiers:
  | RIGHT_DELIM { [] }
  | expression COMMA list_identifiers { $1 :: $3 }
  | EXPRESSION_COMMENT expression COMMA list_identifiers { $4 }
  | expression RIGHT_DELIM { [$1] }
  | EXPRESSION_COMMENT expression RIGHT_DELIM { [] }

expression:
  | INTEGER { Ast.Integer (int_of_string $1) }
  | UPPER_IDENT { Ast.Variable {namev = $1} }
  | functor_elem = functorr { Ast.Functor functor_elem }
  | LEFT_DELIM; expressions = separated_nonempty_list(COMMA, expression); PIPE; tail = expression; RIGHT_DELIM
    { List.fold_right (fun element acc -> (Ast.Functor { namef = ""; elements = [element;acc]; arity = 2 }))
                      expressions
                      tail}
  | LEFT_DELIM; expressions = list_identifiers
    { List.fold_right (fun element acc -> (Ast.Functor { namef = ""; elements = [element;acc]; arity = 2 }))
                      expressions
                      (Ast.Functor { namef = ""; elements = []; arity = 0 })}
