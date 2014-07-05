(* Unison file synchronizer: src/ubase/proplist.ml *)
(* Copyright 1999-2014, Benjamin C. Pierce

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

type 'a key = string
type t = Obj.t Util.StringMap.t

let names = ref Util.StringSet.empty

let register nm =
  if (Util.StringSet.mem nm !names) then
    raise (Util.Fatal
            (Format.sprintf "Property lists: %s already registered!" nm));
  names := Util.StringSet.add nm !names;
  nm

let empty = Util.StringMap.empty

let mem = Util.StringMap.mem

let find (k : 'a key) m : 'a = Obj.obj (Util.StringMap.find k m)

let add (k : 'a key) (v : 'a) m = Util.StringMap.add k (Obj.repr v) m
