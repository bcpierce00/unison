let (major, minor, patch) =
  Scanf.sscanf Sys.ocaml_version "%d.%d.%d" (fun x y z -> (x, y, z))

let compat4pred v = major < 4 || major = 4 && minor < v

let compat4 =
  [
    compat4pred 3, "Compat403", "compat403.cmo";
    compat4pred 8, "Compat408", "compat408.cmo" ]

let objects =
  List.fold_left (fun acc (p, _, n) -> if p then acc ^ " " ^ n else acc) ""

let (flags, objs) =
  match objects compat4 with
  | "" -> ("", "")
  | objs -> ("-pp \"ocaml " ^ Sys.argv.(0)  ^ " pp\"", objs)

(* Compat for OCaml < 4.02 *)
external string_create : int -> string = "caml_create_string"
external input : in_channel -> string -> int -> int -> int = "caml_ml_input"
external output : out_channel -> string -> int -> int -> unit = "caml_ml_output"

let len = 65536

let output_file name =
  let () = set_binary_mode_out stdout true in
  let f = open_in_bin name in
  let s = string_create len in
  let rec loop () =
    match input f s 0 len with
    | 0 -> close_in_noerr f
    | l -> output stdout s 0 l; loop ()
  in loop ()

let same_file n1 n2 =
  String.length n1 > 10 && String.sub n1 0 10 = String.sub n2 0 10

let do_pp filen =
  let fn = Filename.basename filen in
  let stop = ref false in
  List.iter (fun (c, n1, n2) -> if c && not !stop then
    if same_file fn n2 then stop := true
    else print_endline ("open " ^ n1)) compat4;
  print_endline ("# 1 \"" ^ filen ^ "\" 1");
  output_file filen

let () =
  if Array.length Sys.argv > 1 then begin
    match Sys.argv.(1) with
    | "objs" -> print_string objs
    | "pp" -> do_pp Sys.argv.(2)
    | _ -> ()
  end else print_string flags
