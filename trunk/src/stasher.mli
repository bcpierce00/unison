(* Unison file synchronizer: src/stasher.mli *)
(* $I2: Last modified by lescuyer on *)
(* $I3: Copyright 1999-2005 (see COPYING for details) $ *)

(* This module maintains backups for general purpose and *)
(* as archives for mergeable files. *)

(* Backups *)
val initBackups: unit -> unit
val removeAndBackupAsAppropriate: Fspath.t -> Path.local -> Fspath.t -> Path.local -> unit

(* ------------------------ *)
(* Stashes of current versions (so that we have archives when needed for merging) *)

val stashCurrentVersion:
      bool                    (* Do it recursively? *)
   -> Fspath.t                (* fspath to stash *)
   -> Path.local              (* path to stash *)
   -> Path.local option       (* path to actual bits to be stashed (used to stash an 
                                 additional archive version in addition to the current version) *)
   -> unit

val getRecentVersion:
       Fspath.t
    -> Path.local
    -> Os.fullfingerprint
    -> Fspath.t option

(* Return the location of the backup directory *)
val backupDirectory : unit -> Fspath.t

(* Low-level backupdir preference *)
val backupdir : string Prefs.t  
