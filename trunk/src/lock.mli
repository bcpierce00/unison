(* Unison file synchronizer: src/lock.mli *)
(* $Id$ *)
(* Copyright 1999-2006 (see COPYING for details) *)

(* A simple utility module for setting and releasing inter-process locks
   using entries in the filesystem. *)

val acquire : string -> bool
val release : string -> unit
val is_locked : string -> bool
