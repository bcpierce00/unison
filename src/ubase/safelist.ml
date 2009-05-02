(* Unison file synchronizer: src/ubase/safelist.ml *)
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


let filterBoth f l =
  let rec loop r1 r2 = function
    [] -> (List.rev r1, List.rev r2)
  | hd::tl ->
      if f hd then loop (hd::r1) r2 tl
      else loop r1 (hd::r2) tl
  in loop [] [] l

let filterMap f l =
  let rec loop r = function
    [] -> List.rev r
  | hd::tl -> begin
      match f hd with
        None -> loop r tl
      | Some x -> loop (x::r) tl
    end
  in loop [] l

let filterMap2 f l =
  let rec loop r s = function
    [] -> List.rev r, List.rev s
  | hd::tl -> begin
      let (a, b) = f hd in
      let r' = match a with None -> r | Some x -> x::r in
      let s' = match b with None -> s | Some x -> x::s in
      loop r' s' tl
    end
  in loop [] [] l

(* These are tail-recursive versions of the standard ones from the
   List module *)
let rec concat_rec accu =
  function
    [] -> List.rev accu
  | l::r -> concat_rec (List.rev_append l accu) r
let concat l = concat_rec [] l
let flatten = concat

let append l l' =
  match l' with [] -> l | _ -> List.rev_append (List.rev l) l'

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l

let map f l = List.rev (rev_map f l)

let rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match (l1, l2) with
    | ([], []) -> accu
    | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
    | (_, _) -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2
;;

let map2 f l1 l2 = List.rev (rev_map2 f l1 l2)

let rec allElementsEqual = function
    [] -> true
  | [a] -> true
  | a::b::rest -> a=b && (allElementsEqual (b::rest))

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::_ ->
      (* We don't want l to be live when f is called *)
      let l' = List.tl l in
      fold_left f (f accu a) l'

let split l =
  let rec loop acc1 acc2 = function
    [] -> (List.rev acc1, List.rev acc2)
  | (x,y)::l -> loop (x::acc1) (y::acc2) l
  in
    loop [] [] l

let rec transpose_rec accu l =
  match l with
    [] | []::_ ->
      accu
  | [x]::_ ->
      (map (function [x] -> x | _ -> invalid_arg "Safelist.transpose") l)::accu
  | _ ->
      let (l0, r) =
        fold_left
          (fun (l0, r) l1 ->
             match l1 with
               []    -> invalid_arg "Safelist.transpose (2)"
             | a::r1 -> (a::l0, r1::r))
          ([], []) l
      in
      transpose_rec ((List.rev l0)::accu) (List.rev r)

let transpose l = List.rev (transpose_rec [] l)

let combine l1 l2 =
  let rec loop acc = function
    ([], []) -> List.rev acc
  | (a1::l1r, a2::l2r) -> loop ((a1, a2)::acc) (l1r,l2r)
  | (_, _) -> invalid_arg "Util.combine"
  in
    loop [] (l1,l2)

let remove_assoc x l =
  let rec loop acc = function
  | [] -> List.rev acc
  | (a, b as pair) :: rest ->
      if a = x then loop acc rest else loop (pair::acc) rest
  in
    loop [] l

let fold_right f l accu =
  fold_left (fun x y -> f y x) accu (List.rev l)

let flatten_map f l = flatten (map f l)

let remove x l = 
  let rec loop acc = function
  | [] -> List.rev acc
  | a :: rest ->
      if a = x then loop acc rest else loop (a::acc) rest
  in
    loop [] l

let iteri f l =
  let rec loop n = function
    | [] -> ()
    | h::t -> ((f n h); loop (n+1) t)
  in loop 0 l

(* These are already tail recursive in the List module *)
let iter = List.iter
let iter2 = List.iter2
let rev = List.rev
let rev_append = List.rev_append
let hd = List.hd
let tl = List.tl
let nth = List.nth
let length = List.length
let mem = List.mem
let assoc = List.assoc
let for_all = List.for_all
let exists = List.exists
let find = List.find
let filter = List.filter
let stable_sort = List.stable_sort
let sort = List.sort
let partition = List.partition
