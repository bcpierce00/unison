(* $I1: Unison file synchronizer: src/fingerprint.mli $ *)
(* $I2: Last modified by vouillon on Mon, 14 Jun 2004 16:38:56 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

type t

(* Os.safeFingerprint should usually be used rather than these functions *)
val file : Fspath.t -> Path.local -> t
val subfile : string -> Int64.t -> Uutil.Filesize.t -> t

val string : string -> t

val toString : t -> string

(* This dummy fingerprint is guaranteed small and distinct from all
   other fingerprints *)
val dummy : t
