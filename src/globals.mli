(* Unison file synchronizer: src/globals.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

(* Global variables and functions needed by top-level modules and user       *)
(* interfaces                                                                *)

(* The raw names of the roots as specified in the profile or on the command  *)
(* line                                                                      *)
val rawRoots : unit -> string list
val setRawRoots : string list -> unit
val rawRootPair : unit -> string * string

(* Same as [rawRoots], parsed as clroots                                     *)
val parsedClRawRoots : unit -> Clroot.clroot list

(* Parse and canonize roots from their raw names                             *)
val installRoots : (string -> Terminal.termInteract) option -> unit Lwt.t

(* An alternate method (under development?) *)
val installRoots2 : unit -> unit

(* Clear previously installed roots; typically used when switching profiles  *)
val uninstallRoots : unit -> unit

(* The roots of the synchronization (with names canonized, but in the same   *)
(* order as the user gave them)                                              *)
val roots : unit -> Common.root * Common.root

(* same thing, as a list                                                     *)
val rootsList : unit -> Common.root list

(* same thing, but in a standard order and ensuring that a Local root        *)
(* comes first                                                               *)
val rootsInCanonicalOrder : unit -> Common.root list

(* a local root *)
val localRoot : unit -> Common.root

(* Run a command on all roots                                                *)
val allRootsIter :
  (Common.root -> unit Lwt.t) -> unit Lwt.t

(* Run a command on all roots                                                *)
val allRootsIter2 :
  (Common.root -> 'a -> unit Lwt.t) -> 'a list ->
  unit Lwt.t

(* Run a command on all roots and collect results                            *)
val allRootsMap :
  (Common.root -> 'a Lwt.t) -> 'a list Lwt.t

(* Run a command on all roots in parallel, and collect the results.          *)
(* [allRootsMapWIthWaitingAction f wa] calls the function [wa] before        *)
(* waiting for the result for the corresponding root.                        *)
val allRootsMapWithWaitingAction:
    (Common.root -> 'a Lwt.t) -> (Common.root -> unit) -> 'a list Lwt.t

(* The set of paths to synchronize within the replicas                       *)
val paths : Path.t list Prefs.t

(* Expand any paths ending with *                                            *)
val expandWildcardPaths : unit -> unit

(* Make sure that the server has the same settings for its preferences as we
   do locally.  Should be called whenever the local preferences have
   changed.  (This isn't conceptually a part of this module, but it can't
   live in the Prefs module because that would introduce a circular
   dependency.)                                                              *)
val propagatePrefs : unit -> unit Lwt.t

(* User preference: when true, don't ask any questions *)
val batch : bool Prefs.t

(* User preference: ask for confirmation when propagating a deletion of
   a whole replica or top-level path *)
val confirmBigDeletes : bool Prefs.t

(* Predicates on paths *)
val shouldIgnore : 'a Path.path -> bool
val shouldMerge : 'a Path.path -> bool
val ignorePred : Pred.t
val ignorenotPred : Pred.t
val atomic : Pred.t

(* Be careful calling this to add new patterns to be ignored: Its
   value does NOT persist when a new profile is loaded, so it has to
   be called again whenever this happens. *)
val addRegexpToIgnore : string -> unit

(* Merging commands *)
val mergeCmdForPath : Path.t -> string

(* Internal prefs, needed to know whether to do filenames checks *)
val someHostIsRunningWindows : bool Prefs.t
val allHostsAreRunningWindows : bool Prefs.t
val fatFilesystem : bool Prefs.t
