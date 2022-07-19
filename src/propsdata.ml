(* Unison file synchronizer: src/propsdata.ml *)
(* Copyright 2020-2022, Tõivo Leedjärv

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


module type S = sig
  val get : [< `All | `New | `Kept] -> (string * string) list
  val set : (string * string) list -> unit
  val merge : (string * string) list -> unit
  val clear : [`Kept] -> unit
end


module KVStore (V : sig val initSize : int end) = struct

(* Key-value store with a relatively low number of entries (in the tens
   or hundreds, or at most in low thousands).

   This is not a generic key-value store; this is specifically intended
   for use by [Props.Data].

   Several simple implementations are possible (for example, a Map or an
   association list). There seems to be very little difference in terms
   of performance. Hashtbl has been chosen as it may have a slight scaling
   advantage. In practice, there probably are no tangile differences
   between these simple implementations in most scenarios. *)
let mainStore = Hashtbl.create V.initSize
let newStore = Hashtbl.create V.initSize
let keepStore = Hashtbl.create V.initSize

let getStore = function
  | `All -> mainStore
  | `New -> newStore
  | `Kept -> keepStore

let exists key = Hashtbl.mem mainStore key

let find_opt key = Hashtbl.find_opt mainStore key

let associate key value = Hashtbl.add mainStore key value

let associateNew key value =
  associate key value;
  Hashtbl.add newStore key value

let add key value =
  if not (exists key) then associateNew key value

let find key =
  match find_opt key with
  | Some v -> v
  | None -> assert false (* Indicates a bug *)

let get kind =
  Hashtbl.fold (fun key value acc -> (key, value) :: acc) (getStore kind) []

let set d =
  Hashtbl.clear mainStore;
  Hashtbl.clear newStore;
  Safelist.iter (fun (key, value) -> associate key value) d

let associate_cmp key value =
  match find_opt key with
  | None -> associate key value
  | Some v when v = value -> ()
  | Some v ->
      raise (Util.Fatal ("Internal integrity error (propsdata). Key " ^ key
        ^ " returns different results:\n  (existing) " ^ v
        ^ "\nand\n  (new) " ^ value ^ "\n"))

let merge d =
  Safelist.iter (fun (key, value) -> associate_cmp key value) d

let clear kind =
  Hashtbl.clear (getStore kind)

let keep key =
  if Hashtbl.mem keepStore key then ()
  else Hashtbl.add keepStore key (find key)

end (* module KVStore *)


(* ------------------------------------------------------------------------- *)
(*                       Extended attributes (xattr)                         *)
(* ------------------------------------------------------------------------- *)

module Xattr = struct
  include KVStore (struct let initSize = 200 end)

  let length () = Hashtbl.length mainStore
end


(* ------------------------------------------------------------------------- *)
(*                                  ACL                                      *)
(* ------------------------------------------------------------------------- *)

module ACL = KVStore (struct let initSize = 25 end)
