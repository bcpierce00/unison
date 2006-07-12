(* Unison file synchronizer: src/fileutil.mli *)
(* $Id$ *)
(* Copyright 1999-2006 (see COPYING for details) *)

(* Convert backslashes in a string to forward slashes.  Useful in Windows. *)
val backslashes2forwardslashes : string -> string

val removeTrailingSlashes : string -> string
