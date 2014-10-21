(* Unison file synchronizer: src/xferhint.mli *)
(* Copyright 1999-2014, Benjamin C. Pierce (see COPYING for details) *)

(* This module maintains a cache that can be used to map
   an Os.fullfingerprint to a (Fspath.t * Path.t) naming a file that *may*
   (if we are lucky) have this fingerprint.  The cache is not guaranteed
   to be reliable -- the things it returns are only hints, and must be
   double-checked before they are used (to optimize file transfers). *)

val xferbycopying: bool Prefs.t

type handle

(* Suggest a file that's likely to have a given fingerprint *)
val lookup: Os.fullfingerprint -> (Fspath.t * Path.local * handle) option

(* Add a file *)
val insertEntry: Fspath.t -> Path.local -> Os.fullfingerprint -> unit

(* Delete an entry *)
val deleteEntry: handle -> unit
