(* Unison file synchronizer: src/name.mli *)
(* Copyright 1999-2007 (see COPYING for details) *)

type t

val fromString : string -> t
val toString : t -> string

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int
