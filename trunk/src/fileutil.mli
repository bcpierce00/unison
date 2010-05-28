(* Unison file synchronizer: src/fileutil.mli *)
(* Copyright 1999-2010, Benjamin C. Pierce (see COPYING for details) *)

(* Convert backslashes in a string to forward slashes.  Useful in Windows. *)
val backslashes2forwardslashes : string -> string

val removeTrailingSlashes : string -> string
