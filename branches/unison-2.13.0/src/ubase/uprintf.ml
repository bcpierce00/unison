(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

external caml_format_int: string -> int -> string = "caml_format_int"
external caml_format_float: string -> float -> string = "caml_format_float"

let fprintf outchan doafter format =
  let format = (Obj.magic format : string) in
  let rec doprn i =
    if i >= String.length format then
      (doafter(); Obj.magic ())
    else begin
      let c = String.unsafe_get format i in
      if c <> '%' then begin
        output_char outchan c;
        doprn (succ i)
      end else begin
        let j = skip_args (succ i) in
        match String.unsafe_get format j with
          '%' ->
            output_char outchan '%';
            doprn (succ j)
        | 's' ->
            Obj.magic(fun s ->
              if j <= i+1 then
                output_string outchan s
              else begin
                let p =
                  try
                    int_of_string (String.sub format (i+1) (j-i-1))
                  with _ ->
                    invalid_arg "fprintf: bad %s format" in
                if p > 0 && String.length s < p then begin
                  output_string outchan
                                (String.make (p - String.length s) ' ');
                  output_string outchan s
                end else if p < 0 && String.length s < -p then begin
                  output_string outchan s;
                  output_string outchan
                                (String.make (-p - String.length s) ' ')
                end else
                  output_string outchan s
              end;
              doprn (succ j))
        | 'c' ->
            Obj.magic(fun c ->
              output_char outchan c;
              doprn (succ j))
        | 'd' | 'i' | 'o' | 'x' | 'X' | 'u' ->
            Obj.magic(fun n ->
              output_string outchan
                            (caml_format_int (String.sub format i (j-i+1)) n);
              doprn (succ j))
        | 'f' | 'e' | 'E' | 'g' | 'G' ->
            Obj.magic(fun f ->
              output_string outchan
                            (caml_format_float (String.sub format i (j-i+1)) f);
              doprn (succ j))
        | 'b' ->
            Obj.magic(fun b ->
              output_string outchan (string_of_bool b);
              doprn (succ j))
        | 'a' ->
            Obj.magic(fun printer arg ->
              printer outchan arg;
              doprn(succ j))
        | 't' ->
            Obj.magic(fun printer ->
              printer outchan;
              doprn(succ j))
        | c ->
            invalid_arg ("fprintf: unknown format")
      end
    end

  and skip_args j =
    match String.unsafe_get format j with
      '0' .. '9' | ' ' | '.' | '-' -> skip_args (succ j)
    | c -> j

  in doprn 0

let printf doafter fmt = fprintf stdout doafter fmt
and eprintf doafter fmt = fprintf stderr doafter fmt

