(* Unison file synchronizer: src/ubase/proplist.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

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

let names = ref Util.StringMap.empty

let register nm m =
  if (Util.StringMap.mem nm !names) then
    raise (Util.Fatal
            (Format.sprintf "Property lists: %s already registered!" nm));
  names := Util.StringMap.add nm (Obj.repr m) !names;
  nm

let empty = Util.StringMap.empty

let mem = Util.StringMap.mem

let find (k : 'a key) m : 'a = Obj.obj (Util.StringMap.find k m)

let add (k : 'a key) (v : 'a) m = Util.StringMap.add k (Obj.repr v) m

let find_m (k : 'a key) : 'a Umarshal.t =
  try Obj.obj (Util.StringMap.find k !names) with
  | Not_found -> raise (Util.Fatal (Format.sprintf "Property lists: %s not yet registered!" k))

module S = struct
  type key = string
  type value = Obj.t
  type map = t
  let cardinal = Util.StringMap.cardinal
  let empty = Util.StringMap.empty
  let add = Util.StringMap.add
  let iter = Util.StringMap.iter
  let find_m = find_m
end

include Umarshal.Proplist (S)
