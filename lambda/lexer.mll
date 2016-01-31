(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.
*)

{
    exception Eof
}

rule scan = parse
      [' ' '\t']     { scan lexbuf }     (* skip blanks *)
    | ['\n' ]        { Parser.EOL }
    | "\\."          { Parser.LAMBDA }
    | ['0'-'9']+ as lxm { Parser.INT(int_of_string lxm) }
    | "("            { Parser.LBRACE }
    | ")"            { Parser.RBRACE }
    | eof            { raise Eof }
