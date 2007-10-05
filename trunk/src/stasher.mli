(* Unison file synchronizer: src/stasher.mli *)
(* $I2: Last modified by lescuyer on *)
(* $I3: Copyright 1999-2005 (see COPYING for details) $ *)

(* This module maintains backups for general purpose and *)
(* as archives for mergeable files. *)

(* Make a backup copy of a file, if needed; if the third parameter is `AndRemove,
   then the file is either backed up by renaming or deleted if no backup is needed. *)
val backup: Fspath.t -> Path.local -> [`AndRemove | `ByCopying] -> unit

(* Stashes of current versions (so that we have archives when needed for merging) *)
val stashCurrentVersion:
      Fspath.t                (* fspath to stash *)
   -> Path.local              (* path to stash *)
   -> Path.local option       (* path to actual bits to be stashed (used to stash an 
                                 additional archive version in addition to the current version) *)
   -> unit

(* Retrieve a stashed version *)
val getRecentVersion:
       Fspath.t
    -> Path.local
    -> Os.fullfingerprint
    -> Fspath.t option

(* Return the location of the backup directory *)
val backupDirectory : unit -> Fspath.t

(* Check whether current version of a path is being stashed *)
val shouldBackupCurrent : Path.t -> bool

(* Low-level backupdir preference *)
val backupdir : string Prefs.t  

(* Initialize the module *)
val initBackups: unit -> unit

  
