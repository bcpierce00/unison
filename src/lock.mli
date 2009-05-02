(* Unison file synchronizer: src/lock.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* A simple utility module for setting and releasing inter-process locks
   using entries in the filesystem. *)

val acquire : string -> bool
val release : string -> unit
val is_locked : string -> bool
