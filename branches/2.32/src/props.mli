(* Unison file synchronizer: src/props.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* File properties: time, permission, length, etc. *)

type t
val dummy : t
val hash : t -> int -> int
val similar : t -> t -> bool
val override : t -> t -> t
val strip : t -> t
val diff : t -> t -> t
val toString : t -> string
val syncedPartsToString : t -> string
val set : Fspath.t -> Path.local -> [`Set | `Update] -> t -> unit
val get : Unix.LargeFile.stats -> Osx.info -> t
val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> t -> unit
val init : bool -> unit

val same_time : t -> t -> bool
val length : t -> Uutil.Filesize.t
val setLength : t -> Uutil.Filesize.t -> t
val time : t -> float
val setTime : t -> float -> t
val perms : t -> int

val fileDefault : t
val fileSafe : t
val dirDefault : t

val syncModtimes : bool Prefs.t
