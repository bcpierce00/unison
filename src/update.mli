(* Unison file synchronizer: src/update.mli *)
(* Copyright 1999-2010, Benjamin C. Pierce (see COPYING for details) *)

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

(* Retrieve the actual names of the roots *)
val getRootsName : unit -> string 

(* Structures describing dirty files/dirs (1 per path given in the -path preference) *)
val findUpdates :
  unit -> ((Path.local * Common.updateItem * Props.t list) *
           (Path.local * Common.updateItem * Props.t list)) list

(* Take a tree of equal update contents and update the archive accordingly. *)
val markEqual :
  (Name.t * Name.t, Common.updateContent * Common.updateContent) Tree.t -> unit

(* Get and update a part of an archive (the archive remains unchanged) *)
val updateArchive : Fspath.t -> Path.local -> Common.updateItem -> archive
(* Replace a part of an archive by another archive *)
val replaceArchive : Common.root -> Path.t -> archive -> unit Lwt.t
val replaceArchiveLocal : Fspath.t -> Path.local -> archive -> unit
(* Update only some permissions *)
val updateProps :
  Fspath.t -> 'a Path.path -> Props.t option -> Common.updateItem -> unit

(* Check that no updates has taken place in a given place of the filesystem *)
(* Returns an archive mirroring the filesystem contents *)
val checkNoUpdates :
  Fspath.t -> Path.local -> Common.updateItem -> archive

(* Turn off fastcheck for the given file on the next sync. *)
val markPossiblyUpdated : Fspath.t -> Path.local -> unit

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
val useFastChecking : unit -> bool

(* Print the archive to the current formatter (see Format) *)
val showArchive: archive -> unit

(* Compute the size of an update *)
val updateSize : Path.t -> Common.updateItem -> int * Uutil.Filesize.t

(* Iterate on all files in an archive *)
val iterFiles :
  Fspath.t -> Path.local -> archive ->
  (Fspath.t -> Path.local -> Os.fullfingerprint -> unit) -> unit

(* (For breaking the dependency loop between update.ml and stasher.ml...) *)
val setStasherFun : (Fspath.t -> Path.local -> unit) -> unit

(* Conjoin the canonical name of the current host and the canonical
   presentation of the current fspath with the list of names/fspaths of
   all the roots and the current archive format *)
val archiveHash : Fspath.t -> string
