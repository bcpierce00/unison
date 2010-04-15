(* Unison file synchronizer: src/tree.ml *)
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


type ('a, 'b) t =
    Node of ('a * ('a, 'b) t) list * 'b option
  | Leaf of 'b

type ('a, 'b) u =
  { anc: (('a, 'b) u * 'a) option;
    node: 'b option;
    children: ('a * ('a, 'b) t) list}

let start =
  {anc = None; node = None; children = []}

let add t v =
  {t with node = Some v}

let enter t n = {anc = Some (t, n); node = None; children = []}

let leave t =
  match t with
    {anc = Some (t, n); node = None; children = []} ->
      t
  | {anc = Some (t, n); node = Some v; children = []} ->
      {t with children = (n, Leaf v) :: t.children}
  | {anc = Some (t, n); node = v; children = l} ->
      {t with children = (n, (Node (Safelist.rev l, v))) :: t.children}
  | {anc = None} ->
      invalid_arg "Tree.leave"

let finish t =
  match t with
    {anc = Some _} ->
      invalid_arg "Tree.finish"
  | {anc = None; node = Some v; children = []} ->
      Leaf v
  | {anc = None; node = v; children = l} ->
      Node (Safelist.rev l, v)

let rec leave_all t =
  if t.anc = None then t else leave_all (leave t)

let rec empty t =
  {anc =
     begin match t.anc with
       Some (t', n) -> Some (empty t', n)
     | None         -> None
     end;
   node = None;
   children = []}

let slice t =
  (finish (leave_all t), empty t)

(****)

let is_empty t =
  match t with
    Node ([], None) -> true
  | _               -> false

let rec map f g t =
  match t with
    Node (l, v) ->
      Node (Safelist.map (fun (n, t') -> (f n, map f g t')) l,
            match v with None -> None | Some v -> Some (g v))
  | Leaf v ->
      Leaf (g v)

let rec iteri t path pcons f =
  match t with
    Node (l, v) ->
      begin match v with
        Some v -> f path v
      | None   -> ()
      end;
      Safelist.iter (fun (n, t') -> iteri t' (pcons path n) pcons f) l
  | Leaf v ->
      f path v

let rec size_rec s t =
  match t with
    Node (l, v) ->
      let s' = if v = None then s else s + 1 in
      Safelist.fold_left (fun s (_, t') -> size_rec s t') s' l
  | Leaf v ->
      s + 1

let size t = size_rec 0 t

let rec flatten t path pcons result =
  match t with
    Leaf v ->
      (path, v) :: result
  | Node (l, v) ->
      let rem =
        Safelist.fold_right
          (fun (name, t') rem ->
             flatten t' (pcons path name) pcons rem)
          l result
      in
      match v with
        None   -> rem
      | Some v -> (path, v) :: rem
