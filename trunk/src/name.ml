(* Unison file synchronizer: src/name.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


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
    raise (Util.Transient (Printf.sprintf "Filename '%s' contains a '/'" s))
  with Not_found -> () end;
  (* We ought to consider further checks, e.g., in Windows, no colons *)
  s

let hash n =
  Hashtbl.hash (if Case.insensitive () then String.lowercase (Case.normalize n) else n)
