(* Unison file synchronizer: src/ubase/uarg.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* by Xavier Leroy, projet Cristal, INRIA Rocquencourt *)
(* Slightly modified by BCP, July 1999 *)

type spec =
  | Unit of (unit -> unit)     (* Call the function with unit argument *)
  | Set of bool ref            (* Set the reference to true *)
  | Clear of bool ref          (* Set the reference to false *)
  | Bool of (bool -> unit)     (* Pass true to the function *)
  | String of (string -> unit) (* Call the function with a string argument *)
  | Int of (int -> unit)       (* Call the function with an int argument *)
  | Float of (float -> unit)   (* Call the function with a float argument *)
  | Rest of (string -> unit)   (* Stop interpreting keywords and call the
                                  function with each remaining argument *)

exception Bad of string

type error =
  | Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

open Printf

let rec assoc3 x l =
  match l with
  | [] -> raise Not_found
  | (y1, y2, y3)::t when y1 = x -> y2
  | _::t -> assoc3 x t
;;

let usage speclist errmsg =
  printf "%s\n" errmsg;
  Safelist.iter
    (function (key, _, doc) ->
       if String.length doc > 0 && doc.[0] <> '*'
       then printf "  %s %s\n" key doc)
    (Safelist.rev speclist)
;;

let current = ref 0;;

let parse speclist anonfun errmsg =
  let initpos = !current in
  let stop error =
    let progname =
      if initpos < Array.length Sys.argv then Sys.argv.(initpos) else "(?)" in
    begin match error with
      | Unknown s when s = "-help" -> ()
      | Unknown s ->
          eprintf "%s: unknown option `%s'.\n" progname s
      | Missing s ->
          eprintf "%s: option `%s' needs an argument.\n" progname s
      | Wrong (opt, arg, expected) ->
          eprintf "%s: wrong argument `%s'; option `%s' expects %s.\n"
                  progname arg opt expected
      | Message s ->
          eprintf "%s: %s.\n" progname s
    end;
    usage speclist errmsg;
    exit 2;
  in
  let l = Array.length Sys.argv in
  incr current;
  while !current < l do
    let ss = Sys.argv.(!current) in
    if String.length ss >= 1 & String.get ss 0 = '-' then begin
      let args = Util.splitIntoWords ss '=' in
      let s = Safelist.nth args 0 in
      let arg conv mesg =
        match args with
          [_] ->
            if !current + 1 >= l then stop (Missing s) else
             let a = Sys.argv.(!current+1) in
             incr current;
             (try conv a with Failure _ -> stop (Wrong (s, a, mesg)))
        | [_;a] -> (try conv a with Failure _ -> stop (Wrong (s, a, mesg)))
        | _ -> stop (Message (sprintf "Garbled argument %s" s)) in
      let action =
        try assoc3 s speclist
        with Not_found -> stop (Unknown s)
      in
      begin try
        match action with
        | Unit f -> f ();
        | Set r -> r := true;
        | Clear r -> r := false;
        | Bool f ->
            begin match args with
              [_] -> f true
            | _   -> f (arg bool_of_string "a boolean")
            end
        | String f -> f (arg (fun s-> s) "")
        | Int f    -> f (arg int_of_string "an integer")
        | Float f  -> f (arg float_of_string "a float")
        | Rest f ->
            while !current < l-1 do
              f Sys.argv.(!current+1);
              incr current;
            done;
      with Bad m -> stop (Message m);
      end;
      incr current;
    end else begin
      (try anonfun ss with Bad m -> stop (Message m));
      incr current;
    end;
  done;
;;
