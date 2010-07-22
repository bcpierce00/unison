(* Unison file synchronizer: src/fingerprint.mli *)
(* Copyright 1999-2010, Benjamin C. Pierce (see COPYING for details) *)

type t

(* Os.safeFingerprint should usually be used rather than these functions *)
val file : Fspath.t -> Path.local -> t
val subfile : Fspath.t -> Int64.t -> Uutil.Filesize.t -> t

val string : string -> t

val toString : t -> string

(* This dummy fingerprint is guaranteed small and distinct from all
   other fingerprints *)
val dummy : t

val hash : t -> int
val equal : t -> t -> bool

(* A pseudo-fingerprint has the same type as a real one (so it can
   be stored in the archive, etc.), but it is computed just from the
   size of the file, ignoring the contents *)
val pseudo : Uutil.Filesize.t -> t
val ispseudo : t -> bool
