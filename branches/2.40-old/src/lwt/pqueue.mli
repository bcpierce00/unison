(* Unison file synchronizer: src/lwt/pqueue.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

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

module Make(Ord: OrderedType) : S with type elt = Ord.t
