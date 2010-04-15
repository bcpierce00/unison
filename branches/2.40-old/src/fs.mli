(* Unison file synchronizer: src/fs.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* Operations on fspaths *)

include System_intf.Core with type fspath = Fspath.t

val digestFile : Fspath.t -> string

val setUnicodeEncoding : bool -> unit
