(* Unison file synchronizer: src/fpcache.mli *)
(* Copyright 1999-2012, Benjamin C. Pierce (see COPYING for details) *)

(* Initialize the cache *)
val init : bool -> bool -> System.fspath -> unit

(* Close the cache file and clear the in-memory cache *)
val finish : unit -> unit

(* Get the fingerprint of a file, possibly from the cache *)
val fingerprint :
  ?newfile:bool ->
  bool -> Fspath.t -> Path.local -> Fileinfo.t -> Os.fullfingerprint option ->
  Props.t * Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp

(* Add an entry to the cache *)
val save :
  Path.local ->
  Props.t * Os.fullfingerprint * Fileinfo.stamp * Osx.ressStamp -> unit

(****)

val dataClearlyUnchanged :
  bool -> Path.local -> Fileinfo.t -> Props.t -> Fileinfo.stamp -> bool
val ressClearlyUnchanged :
  bool -> Fileinfo.t -> 'a Osx.ressInfo -> bool -> bool
(* Is that a file for which fast checking is disabled? *)
val excelFile : Path.local -> bool
