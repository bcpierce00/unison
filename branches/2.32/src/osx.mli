(* Unison file synchronizer: src/osx.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

val init : bool -> unit
val isMacOSX : bool

val rsrc : bool Prefs.t

type 'a ressInfo
type ressStamp = unit ressInfo
type info =
  { ressInfo : (string * int64) ressInfo;
    finfo : string }

val getFileInfos : Fspath.t -> Path.local -> [> `DIRECTORY | `FILE ] -> info
val setFileInfos : Fspath.t -> Path.local -> string -> unit

val ressUnchanged :
  'a ressInfo -> 'b ressInfo -> float option -> bool -> bool

val ressFingerprint : Fspath.t -> Path.local -> info -> Fingerprint.t
val ressLength : 'a ressInfo -> Uutil.Filesize.t

val ressDummy : ressStamp
val ressStampToString : ressStamp -> string

val stamp : info -> ressStamp

val appleDoubleFile : Fspath.t -> Path.local -> string

val openRessIn : Fspath.t -> Path.local -> in_channel
val openRessOut : Fspath.t -> Path.local -> Uutil.Filesize.t -> out_channel
