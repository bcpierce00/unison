(* Unison file synchronizer: src/fswatch.mli *)
(* Copyright 1999-2012, Benjamin C. Pierce (see COPYING for details) *)

type archiveHash = string

val start : archiveHash -> bool

val startScanning : archiveHash -> Fspath.t -> Path.local -> unit
val stopScanning : unit -> unit
val scanDirectory : Path.local -> unit
val followLink : Path.local -> unit

val wait : archiveHash -> unit Lwt.t
val getChanges : archiveHash -> Path.t list

(****)

val useWatcher : bool Prefs.t
