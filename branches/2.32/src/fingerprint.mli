(* Unison file synchronizer: src/fingerprint.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

type t

(* Os.safeFingerprint should usually be used rather than these functions *)
val file : Fspath.t -> Path.local -> t
val subfile : string -> Int64.t -> Uutil.Filesize.t -> t

val string : string -> t

val toString : t -> string

(* This dummy fingerprint is guaranteed small and distinct from all
   other fingerprints *)
val dummy : t
