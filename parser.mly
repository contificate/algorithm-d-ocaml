
%token<string> IDENT
%token LPAR RPAR WILDCARD COMMA EOF

%type<Ast.Tree.t> app
%start<Ast.Tree.t> start

%%

start:
  | a = app EOF { a }

app:
  | WILDCARD { Wildcard }
  | f = IDENT { Node (f, []) }
  | f = IDENT LPAR xs = separated_list(COMMA, app) RPAR { Node (f, xs) } 

