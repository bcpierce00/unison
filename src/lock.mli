(* $I1: Unison file synchronizer: src/lock.mli $ *)
(* $I2: Last modified by bcpierce on Sun, 24 Mar 2002 11:24:03 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

(* A simple utility module for setting and releasing inter-process locks
   using entries in the filesystem. *)

val acquire : string -> bool
val release : string -> unit
val is_locked : string -> bool
