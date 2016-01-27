/* File parser.mly */

%token EOL
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token ZERO
%token SUCC
%token PRED
%token ISZERO
%token LBRACE
%token RBRACE
%start parse
%type <int> parse
%type <int> expr
%%

parse:
      EOL       { -1 }
    | expr EOL  { $1 }
;

expr:
      ZERO  { 0 }
    | SUCC expr { $2 + 1 }
    | PRED expr { $2 - 1 }
    | LBRACE SUCC expr RBRACE { $3 + 1 }
    | LBRACE PRED expr RBRACE { $3 - 1 }
    | IF expr THEN expr ELSE expr { if $2 > 0 then $4 else $6 }
    | TRUE {1}
    | FALSE {0}
    | ISZERO expr {if $2 == 0 then 1 else 0}
;

