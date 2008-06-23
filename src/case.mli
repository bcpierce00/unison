(* Unison file synchronizer: src/case.mli *)
(* Copyright 1999-2008 (see COPYING for details) *)

val insensitive : unit -> bool

val normalize : string -> string

val init : bool -> unit
