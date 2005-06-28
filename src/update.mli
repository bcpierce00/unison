(* $I1: Unison file synchronizer: src/update.mli $ *)
(* $I2: Last modified by tjim on Tue, 14 Sep 2004 11:51:02 -0400 $ *)
(* $I3: Copyright 1999-2004 (see COPYING for details) $ *)

module NameMap : Map.S with type key = Name.t

type archive =
    ArchiveDir of Props.t * archive NameMap.t
  | ArchiveFile of Props.t * Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp
  | ArchiveSymlink of string
  | NoArchive

(* Calculate a canonical name for the set of roots to be synchronized.  This
   will be used in constructing the archive name for each root. Note, all
   the roots in this canonical name will contain hostnames, even local
   roots, so the roots are re-sorted. *)
val storeRootsName : unit -> unit

val findOnRoot :
  Common.root -> Path.t list -> Common.updateItem list Lwt.t

val findUpdates :
  unit -> Common.updateItem list Common.oneperpath
          (* Structures describing dirty files/dirs (1 per given path) *)

(* Take a tree of equal update contents and update the archive accordingly. *)
val markEqual :
  (Name.t, Common.updateContent * Common.updateContent) Tree.t -> unit

(* Commit in memory the last archive updates, or rollback if an exception is
   raised.  A commit function must have been specified on both sides before
   finishing the transaction. *)
type transaction
val transaction : (transaction -> unit Lwt.t) -> unit Lwt.t

(* Update a part of an archive *)
val updateArchive :
  Common.root -> Path.t -> Common.updateItem -> transaction ->
  (Path.local * archive) Lwt.t
(* Replace a part of an archive by another archive *)
val replaceArchive :
  Common.root -> Path.t -> (Fspath.t * Path.local) option ->
  archive -> transaction -> Path.local Lwt.t
(* Update only some permissions *)
val updateProps :
  Common.root -> Path.t -> Props.t option -> Common.updateItem ->
  transaction -> Path.local Lwt.t

(* Check that no updates has taken place in a given place of the filesystem *)
val checkNoUpdates :
 Common.root -> Path.t -> Common.updateItem -> unit Lwt.t

(* Save to disk the archive updates *)
val commitUpdates : unit -> unit

(* In the user interface, it's helpful to know whether unison was started
   with no archives.  (Then we can display file status as 'unknown' rather
   than 'new', which seems friendlier for new users.)  This flag gets set
   false by the crash recovery code when it determines that no archives were
   present. *)
val foundArchives : bool ref

(* Unlock the archives, if they are locked. *)
val unlockArchives : unit -> unit Lwt.t

(* Translate a global path into a local path using the archive *)
val translatePath : Common.root -> Path.t -> Path.local Lwt.t
val translatePathLocal : Fspath.t -> Path.t -> Path.local

(* Are we checking fast, or carefully? *)
val fastcheck : string Prefs.t

(* Print the archive to the current formatter (see Format) *)
val showArchive: archive -> unit

(* Internal prefs, needed to know whether to do filenames checks *)
val someHostIsRunningWindows : bool Prefs.t
val allHostsAreRunningWindows : bool Prefs.t
