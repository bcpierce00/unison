open Printf 

let main() = begin

(* The structure *)
let ml = open_out_bin "../src/strings.ml" in
fprintf ml "(* DO NOT MODIFY.\n\
\032  This file has been automatically generated, see docs.ml. *)\n\n";

(* Process the manual *)
let rec findFirstSNIP ch =
  try
    let l = input_line ch in
    if l <> "----SNIP----" then findFirstSNIP ch 
  with
    End_of_file ->
      (Printf.printf "File does not contain ----SNIP----\n";
       exit 1) in

let prsection ch =
  let name = input_line ch in
  let shortname = input_line ch in
  let empty = input_line ch in
  if empty<>"" then
    (fprintf stderr "Second line after SNIP is '%s', not empty!\n" empty;
     exit 1);
  fprintf ml "    (\"%s\", (\"%s\", \n     \"" shortname name;
  let rec loop () =
    let l = input_line ch in
    if l<>"----SNIP----" then begin
      for n=0 to (String.length l) - 1 do
        let e =
          if n=0 & l.[n]=' ' then "\\032"
          else if l.[n]='"' then "\\\""
          else if l.[n]='\'' then "'"
          else if (Char.code l.[n])>=128 then sprintf "\\%d" (Char.code l.[n])
          else Char.escaped l.[n] in
        output_string ml e;
      done;
      fprintf ml "\\n\\\n      ";
      loop()
    end in
  (try loop() with End_of_file -> ());  
  fprintf ml "\"))\n::\n" in

let prmanual() = 
  fprintf ml "let docs =\n";
  let ch = open_in "../doc/unison-manual.dtxt" in
  findFirstSNIP ch;
  try
    while true do prsection ch done
  with End_of_file -> ();
  close_in ch;  
  fprintf ml "    [];;\n\n" in

(* $Format: "let myName = \"$Project$\" in"$ *)
let myName = "unison" in

(* Docs *)
prmanual ();

(* Clean up *)  
close_out ml;

end (* of main *);;
(*--------------------------------------------------------------------------*)

Printexc.catch main ();;


