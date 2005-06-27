(* $I1: Unison file synchronizer: src/name.mli $ *)
(* $I2: Last modified by vouillon on Wed, 17 Apr 2002 12:03:26 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

type t

val fromString : string -> t
val toString : t -> string

val compare : t -> t -> int
val eq : t -> t -> bool
val hash : t -> int
