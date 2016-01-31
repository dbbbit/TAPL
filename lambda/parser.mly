%{
(* File parser.mly *)
open Syntax
%}

%token EOL
%token LAMBDA
%token <int> INT
%token LBRACE
%token RBRACE
%start parse
%type <Syntax.term> parse
%type <Syntax.term> app
%type <Syntax.term> value
%%

parse:
    | app EOL { $1 }

app:
    | app value { TmApp($1, $2) }
    | value { $1 }

value:
    | LBRACE app RBRACE { $2 }
    | LAMBDA app { TmAbs($2) }
    | INT { TmVar($1) }
