(*
This file is taken from the Objective Caml standard library.
Some functions have been added to suit Unison's needs.
*)
(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. 
*)

module type OrderedType = 
  sig
    type t
      (** The type of the map keys. *)
    val compare : t -> t -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Pervasives.compare}. *)
  end
(** Input signature of the functor {!Map.Make}. *)

module type S =
  sig
    type key
    (** The type of the map keys. *)

    type (+'a) t
    (** The type of maps from type [key] to type ['a]. *)

    val empty: 'a t
    (** The empty map. *)

    val is_empty: 'a t -> bool
    (** Test whether a map is empty or not. *)

    val add: key -> 'a -> 'a t -> 'a t
    (** [add x y m] returns a map containing the same bindings as
       [m], plus a binding of [x] to [y]. If [x] was already bound
       in [m], its previous binding disappears. *)

    val find: key -> 'a t -> 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val findi: key -> 'a t -> key * 'a
    (** [find x m] returns the current binding of [x] in [m],
       or raises [Not_found] if no such binding exists. *)

    val remove: key -> 'a t -> 'a t
    (** [remove x m] returns a map containing the same bindings as
       [m], except for [x] which is unbound in the returned map. *)

    val mem: key -> 'a t -> bool
    (** [mem x m] returns [true] if [m] contains a binding for [x],
       and [false] otherwise. *)

    val iter: (key -> 'a -> unit) -> 'a t -> unit
    (** [iter f m] applies [f] to all bindings in map [m].
       [f] receives the key as first argument, and the associated value
       as second argument.  The bindings are passed to [f] in increasing
       order with respect to the ordering over the type of the keys.
       Only current bindings are presented to [f]:
       bindings hidden by more recent bindings are not passed to [f]. *)

    val map: ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] returns a map with same domain as [m], where the
       associated value [a] of all bindings of [m] has been
       replaced by the result of the application of [f] to [a].
       The bindings are passed to [f] in increasing order
       with respect to the ordering over the type of the keys. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val mapii: (key -> 'a -> key * 'b) -> 'a t -> 'b t
    (** Same as {!Map.S.map}, but the function receives as arguments both the
       key and the associated value for each binding of the map. *)

    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
       where [k1 ... kN] are the keys of all bindings in [m]
       (in increasing order), and [d1 ... dN] are the associated data. *)

    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    (** Total ordering between maps.  The first argument is a total ordering
        used to compare data associated with equal keys in the two maps. *)

    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
       equal, that is, contain equal keys and associate them with
       equal data.  [cmp] is the equality predicate used to compare
       the data associated with the keys. *)

    val validate: 'a t -> [`Ok | `Duplicate of key | `Invalid of key * key]
  end
(** Output signature of the functor {!Map.Make}. *)

module Make (Ord : OrderedType) : S with type key = Ord.t
(** Functor building an implementation of the map structure
   given a totally ordered type. *)
