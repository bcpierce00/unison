(* Unison file synchronizer: src/features.ml *)
(* Copyright 2021, Tõivo Leedjärv

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

type id = string

type t = { mutable enabled : bool;
           validator : (id list -> bool -> string option) option }

let allFeatures = Hashtbl.create 8
let allNames = ref []

let all () = !allNames

let mem = List.mem

let empty = []

let inter a b = List.filter (fun name -> mem name a) b

let setEnabled features =
  Hashtbl.iter (fun name t -> t.enabled <- mem name features) allFeatures

let resetEnabled () = setEnabled empty

(***************)

let validate features =
  let aux name t =
    let failed = match t.validator with
      | Some fn -> fn features (mem name features)
      | None -> None
    in
    match failed with
    | None -> ()
    | Some e ->
        raise (Util.Fatal
          ("Client and server are incompatible. Setting up feature \""
           ^ name ^ "\" failed with error\n\"" ^ e ^ "\".\n\n"
           ^ "It may be possible to rectify this by changing the user "
           ^ "preferences.\nUltimately, it may require upgrading either "
           ^ "the server or the client."))
  in
  Hashtbl.iter aux allFeatures

let validateEnabled () =
  let enabled name t accu = if t.enabled then name :: accu else accu in
  validate (Hashtbl.fold enabled allFeatures [])

(***************)

let enabled feature = feature.enabled

let dummy = { enabled = false; validator = None }

let register name validatefn =
  if Hashtbl.mem allFeatures name then
    raise (Util.Fatal ("Feature " ^ name ^ " registered twice"));
  let v = { enabled = false; validator = validatefn } in
  Hashtbl.add allFeatures name v;
  allNames := name :: !allNames;
  v

