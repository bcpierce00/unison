(* $I1: Unison file synchronizer: src/name.ml $ *)
(* $I2: Last modified by vouillon on Wed, 17 Apr 2002 12:03:26 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

(* NOTE: IF YOU CHANGE TYPE "NAME", THE ARCHIVE FORMAT CHANGES;
   INCREMENT "UPDATE.ARCHIVEFORMAT" *)
type t = string

let compare n1 n2 =
  if Case.insensitive () then
    Util.nocase_cmp (Case.normalize n1) (Case.normalize n2)
  else
    compare n1 n2

let eq a b = (0 = (compare a b))

let toString n = n

let fromString s =
  if String.length s = 0 then
    raise(Invalid_argument "Name.fromString(empty string)");
  (* Make sure there are no slashes in the s *)
  begin try
    ignore(String.index s '/');
    raise(Invalid_argument (Printf.sprintf
      "Name.fromString('%s' contains a '/')" s))
  with Not_found -> () end;
  (* We ought to consider further checks, e.g., in Windows, no colons *)
  s

let hash n =
  Hashtbl.hash (if Case.insensitive () then String.lowercase n else n)
