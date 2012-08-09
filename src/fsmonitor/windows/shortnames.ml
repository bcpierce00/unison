(* Unison file synchronizer: src/monitoring-linux/lwt_inotify.ml *)
(* Copyright 2012, Benjamin C. Pierce 

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

type handle

external findnext_short : handle -> string * string = "win_findnext_short"
external findfirst_short : string -> (string * string) * handle =
  "win_findfirst_short"
external findclose : handle -> unit = "win_findclosew"

let epath = System_impl.Fs.W.epath
let path8 = System_impl.Fs.W.path8

let convert (l, s) = (path8 l, path8 s)

let of_file f =
  try
    let (m, h) = findfirst_short (epath f) in
    findclose h;
    Some (convert m)
  with End_of_file ->
    None

let in_directory d =
  let l = ref [] in
  try
    let (first_entry, handle) =
      findfirst_short (epath (Filename.concat d "*")) in
    l := [convert first_entry];
    while true do
      l := convert (findnext_short handle) :: !l
    done;
    assert false
  with End_of_file ->
    !l

