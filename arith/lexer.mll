(* 
   The lexical analyzer: lexer.ml is generated automatically
   from lexer.mll.

   Ref: http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html
   
   The only modification commonly needed here is adding new keywords to the 
   list of reserved words at the top.  
*)

{
    exception Eof
    open String
}

rule scan = parse
      [' ' '\t']     { scan lexbuf }     (* skip blanks *)
    | ['\n' ]        { Parser.EOL }
    | "if"           { Parser.IF }
    | "then"         { Parser.THEN }
    | "else"         { Parser.ELSE }
    | "true"         { Parser.TRUE }
    | "false"        { Parser.FALSE }
    | "0"            { Parser.ZERO }
    | "succ"         { Parser.SUCC }
    | "pred"         { Parser.PRED }
    | "iszero"       { Parser.ISZERO }
    | "("            { Parser.LBRACE }
    | ")"            { Parser.RBRACE }
    | eof            { raise Eof }
