(* Unison file synchronizer: src/case.mli *)
(* $Id$ *)
(* Copyright 1999-2007 (see COPYING for details) *)

val insensitive : unit -> bool

val normalize : string -> string

val init : bool -> unit
