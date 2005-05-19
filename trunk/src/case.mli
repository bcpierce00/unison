(* $I1: Unison file synchronizer: src/case.mli $ *)
(* $I2: Last modified by zheyang on Wed, 12 Dec 2001 02:26:21 -0500 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

val insensitive : unit -> bool

val normalize : string -> string

val init : bool -> unit
