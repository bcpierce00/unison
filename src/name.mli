(* Unison file synchronizer: src/name.mli *)
(* $Id: $ *)
(* Copyright 1999-2006 (see COPYING for details) *)

type t

val fromString : string -> t
val toString : t -> string

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int
