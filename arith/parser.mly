%{
(* File parser.mly *)
open Syntax
%}

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
%type <Syntax.term> parse
%type <Syntax.term> expr
%%

parse:
      EOL       { TmFalse }
    | expr EOL  { $1 }
;

expr:
      ZERO  { TmZero }
    | SUCC expr { TmSucc($2) }
    | PRED expr { TmPred($2) }
    | LBRACE expr RBRACE { $2 }
    | IF expr THEN expr ELSE expr { TmIf($2, $4, $6) }
    | TRUE { TmTrue }
    | FALSE { TmFalse }
    | ISZERO expr { TmIsZero($2) }
;

