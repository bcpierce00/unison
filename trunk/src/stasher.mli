(* $I1: Unison file synchronizer: src/stasher.mli $ *)
(* $I2: Last modified by lescuyer on *)
(* $I3: Copyright 1999-2005 (see COPYING for details) $ *)

(* This module maintains backups for general purpose and *)
(* as archives for mergeable files. *)

(* Archives for merge purposes *)
(* val stashCurrentVersion: Path.local -> unit *)
val stashCurrentVersion: Fspath.t -> Path.local -> unit
val getRecentVersion: Fspath.t -> Path.local -> Os.fullfingerprint -> Fspath.t option

(* Backups *)
val initBackups: unit -> unit
val removeAndBackupAsAppropriate: Fspath.t -> Path.local -> Fspath.t -> Path.local -> unit
