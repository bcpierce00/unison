(* Unison file synchronizer: src/xferhint.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* This module maintains a cache that can be used to map
   an Os.fingerprint to a (Fspath.t * Path.t) naming a file that *may*
   (if we are lucky) have this fingerprint.  The cache is not guaranteed
   to be reliable -- the things it returns are only hints, and must be
   double-checked before they are used (to optimize file transfers). *)

val xferbycopying: bool Prefs.t

(* Suggest a file that's likely to have a given fingerprint *)
val lookup: Os.fullfingerprint -> (Fspath.t * Path.local) option

(* Add, delete, and rename entries *)
val insertEntry: Fspath.t * Path.local -> Os.fullfingerprint -> unit
val deleteEntry: Fspath.t * Path.local -> unit
val renameEntry: Fspath.t * Path.local -> Fspath.t * Path.local -> unit
