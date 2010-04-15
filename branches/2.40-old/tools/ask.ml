let args = Sys.argv;;

let n = args.(1);;

let ch = open_in n;;

let rec grabtext t =
  try
    let l = input_line ch in
    grabtext(t ^ l ^ "\n")
  with 
    End_of_file -> t;;

print_string (grabtext "");;
print_string "\nPress return to continue... \n";;
flush stdout;;

let _ = input_line stdin;;



