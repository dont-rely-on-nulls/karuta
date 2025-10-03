%{
%}

%token <string> LITERAL_ATOM
%token <string> IDENT
%token <string> INTEGER
%token <string> UPPER_IDENT
%token LEFT_DELIM
%token RIGHT_DELIM
%token COMMA
%token DOT
%token HOLDS
%token EOF
%token QUERY

%start <Ast.ParserClause.t list> program
%%

program:
  | declaration program
    { ($1 :: $2) }
  | query program
    { ($1 :: $2) }
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

declaration:
  | functor_elem = functorr; DOT
    { Ast.Declaration {head = functor_elem; body = []}}
  | functor_elem = functorr; HOLDS; statements = separated_nonempty_list(COMMA, functorr); DOT
    { Ast.Declaration { head = functor_elem; body = statements } }
  ;

query:
  | functor_elem = functorr; QUERY
    { Ast.Query functor_elem}
  ;

list_identifiers:
  | RIGHT_DELIM { [] }
  | expression COMMA list_identifiers { $1 :: $3 }
  | expression RIGHT_DELIM { [$1] }

expression:
  | INTEGER { Ast.Integer (int_of_string $1) }
  | UPPER_IDENT { Ast.Variable {namev = $1} }
  | functor_elem = functorr { Ast.Functor functor_elem }
