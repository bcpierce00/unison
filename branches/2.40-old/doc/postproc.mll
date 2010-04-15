(*
 * Convert <br>\n<br> to <p>
 *)

{ }

rule lex = parse
  eof { }

| "<BR>\n<BR>" { print_string "<p>"; lex lexbuf }
| "<BR><BR>" { print_string "<p>"; lex lexbuf }
    
| _ { print_string (Lexing.lexeme lexbuf); lex lexbuf }

{
  let () = lex (Lexing.from_channel stdin)
}
