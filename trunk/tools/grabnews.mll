(*
 * Dump current changes from PRCS .prj file
 *)

rule init = parse
  eof { output_string stderr "\nCouldn't find New-Version-Log!\n\n";
        flush stderr;
        exit 1; }
| "(New-Version-Log \"\")"
      { output_string stderr "\nNew-Version-Log is empty!\n\n";
        flush stderr;
        exit 1; }
| "(New-Version-Log \""
      { eatNgrab lexbuf; }
| _   { init lexbuf; }

and eatNgrab = parse
  "\n" | ' ' { eatNgrab lexbuf; }  
| _          { print_string (Lexing.lexeme lexbuf); grab lexbuf; }

and grab = parse
  "\")\n"
      { print_string "\n";
        flush stdout;
        exit 0; }  
| "\n\")\n"
      { flush stdout;
        exit 0; }  
| _   { print_string (Lexing.lexeme lexbuf);
        grab lexbuf; }

{
  let () = init (Lexing.from_channel stdin)
}
