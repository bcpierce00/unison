(* Unison file synchronizer: src/lwt/pqueue.ml *)
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


module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val add: elt -> t -> t
    val union: t -> t -> t
    val find_min: t -> elt
    val remove_min: t -> t
  end

module Make(Ord: OrderedType) : (S with type elt = Ord.t) =
  struct
    type elt = Ord.t

    type t = tree list
    and tree = Node of elt * int * tree list

    let root (Node (x, _, _)) = x
    let rank (Node (_, r, _)) = r
    let link (Node (x1, r1, c1) as t1) (Node (x2, r2, c2) as t2) =
      let c = Ord.compare x1 x2 in
      if c <= 0 then Node (x1, r1 + 1, t2::c1) else Node(x2, r2 + 1, t1::c2)
    let rec ins t =
      function
        []     ->
          [t]
      | (t'::_) as ts when rank t < rank t' ->
          t::ts
      | t'::ts ->
          ins (link t t') ts

    let empty = []
    let is_empty ts = ts = []
    let add x ts = ins (Node (x, 0, [])) ts
    let rec union ts ts' =
      match ts, ts' with
        ([], _) -> ts'
      | (_, []) -> ts
      | (t1::ts1, t2::ts2)  ->
          if rank t1 < rank t2 then t1 :: union ts1 (t2::ts2)
          else if rank t2 < rank t1 then t2 :: union (t1::ts1) ts2
          else ins (link t1 t2) (union ts1 ts2)

    let rec find_min =
      function
        []    -> raise Not_found
      | [t]   -> root t
      | t::ts ->
          let x = find_min ts in
          let c = Ord.compare (root t) x in
          if c < 0 then root t else x

    let rec get_min =
      function
        []    -> assert false
      | [t]   -> (t, [])
      | t::ts ->
          let (t', ts') = get_min ts in
          let c = Ord.compare (root t) (root t') in
          if c < 0 then (t, ts) else (t', t::ts')

    let remove_min =
      function
        [] -> raise Not_found
      | ts ->
          let (Node (x, r, c), ts) = get_min ts in
          union (List.rev c) ts
  end
