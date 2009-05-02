(* Unison file synchronizer: src/fileutil.ml *)
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


(* Convert backslashes in a string to forward slashes.  Useful in Windows.   *)
let backslashes2forwardslashes s0 =
  try
    ignore(String.index s0 '\\'); (* avoid alloc if possible *)
    let n = String.length s0 in
    let s = String.create n in
    for i = 0 to n-1 do
      let c = String.get s0 i in
      if c = '\\'
      then String.set s i '/'
      else String.set s i c
    done;
    s
  with Not_found -> s0

let rec removeTrailingSlashes s =
  let len = String.length s in
  if len>0 && String.get s (len-1) = '/'
  then removeTrailingSlashes (String.sub s 0 (len-1))
  else s
