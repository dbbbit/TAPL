let _ =
    try
        let lexbuf = Lexing.from_channel stdin in
        while true do
            let result = Parser.parse Lexer.scan lexbuf in
            Syntax.print_term (Syntax.eval result);
            print_newline();
            flush stdout
        done
    with Lexer.Eof ->
        exit 0
