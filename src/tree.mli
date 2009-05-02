(* Unison file synchronizer: src/tree.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* An ('a, 'b) t is a tree with 'a-labeled arcs and 'b-labeled nodes.        *)
(* Labeling for the internal nodes is optional                               *)
type ('a, 'b) t =
    Node of ('a * ('a, 'b) t) list * 'b option
  | Leaf of 'b

(* An "unfinished" tree                                                      *)
type ('a, 'b) u

(* ------------------------------------------------------------------------- *)
(*                   Functions for unfinished tree (u-tree)                  *)
(* ------------------------------------------------------------------------- *)

(* start an empty u-tree                                                     *)
val start : ('a, 'b) u

(* add t v: add a node with label "v" at the current position                *)
val add : ('a, 'b) u -> 'b -> ('a, 'b) u

(* enter t n: create a new subtree, with leading arc labeled "v", at the     *)
(* current position                                                          *)
val enter : ('a, 'b) u -> 'a -> ('a, 'b) u

(* go up one-level                                                           *)
val leave : ('a, 'b) u -> ('a, 'b) u

(* ------------------------------------------------------------------------- *)
(*                       From u-trees to trees                               *)
(* ------------------------------------------------------------------------- *)

(* "finish" up the tree construction and deliver a tree precondition:        *)
(* already at the top-level of the tree                                      *)
val finish : ('a, 'b) u -> ('a, 'b) t

(* from the u-tree, deliver a tree (by going back to top-level and "finish") *)
(* and the skeleton u-tree, which represents the current position            *)
val slice : ('a, 'b) u -> ('a, 'b) t * ('a, 'b) u

(* ------------------------------------------------------------------------- *)
(*                          Functions for trees                              *)
(* ------------------------------------------------------------------------- *)

(* Test if the tree is empty                                                 *)
val is_empty : ('a, 'b) t -> bool

(* pointwise renaming of arcs and nodes                                      *)
val map : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t

(* DFT the tree, keeping an accumulator for the path, and apply a function   *)
(* to all the partial paths ended by a labeled node                          *)
val iteri : ('a, 'b) t -> 'c -> ('c -> 'a -> 'c) -> ('c -> 'b -> unit) -> unit

(* count the number of labeled nodes                                         *)
val size : ('a, 'b) t -> int

(* DFT the tree, keep an accumulator for the path, and record all the        *)
(* partial paths ended by a labeled node                                     *)
val flatten :
  ('a, 'b) t -> 'c -> ('c -> 'a -> 'c) -> ('c * 'b) list -> ('c * 'b) list
