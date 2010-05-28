(* Unison file synchronizer: src/name.ml *)
(* Copyright 1999-2010, Benjamin C. Pierce 

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

let compare n1 n2 = (Case.ops())#compare n1 n2

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

let hash n = (Case.ops())#hash n

let normalize n = (Case.ops())#normalizeFilename n

(****)

let badEncoding s = (Case.ops())#badEncoding s

(* Windows file naming conventions are descripted here:
   <http://msdn.microsoft.com/en-us/library/aa365247(printer).aspx> *)
let badWindowsFilenameRx =
  Rx.case_insensitive
    (Rx.rx
       "(.*[\000-\031<>:\"/\\|?*].*)|\
        ((con|prn|aux|nul|com[1-9]|lpt[1-9])(\\..*)?)|\
        (.*[. ])")

let badWindowsFilenameRelaxedRx =
  Rx.case_insensitive (Rx.rx "(con|prn|aux|nul|com[1-9]|lpt[1-9])(\\..*)?")

(* FIX: should also check for a max filename length, not sure how much *)
let badFile s = Rx.match_string badWindowsFilenameRx s
