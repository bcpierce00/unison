(* Unison file synchronizer: src/props.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

(* File properties: time, permission, length, etc. *)

type t251
type _ props
type basic = [`Basic] props
type t = [`Full] props
type x = [`ExtLoaded] props
val m : t Umarshal.t
val mbasic : basic Umarshal.t
val mx : x Umarshal.t
val to_compat251 : _ props -> t251
val of_compat251 : t251 -> _ props
val dummy : _ props
val hash : t -> int -> int
val hash251 : t251 -> int -> int
val similar : t -> t -> bool
val override : _ props -> 'a props -> 'a props
val strip : t -> t
val diff : t -> x -> x
val toString : t -> string
val syncedPartsToString : t -> string
val set : Fspath.t -> Path.local -> [`Set | `Update] -> x -> unit
val get' : Unix.LargeFile.stats -> basic
val get : ?archProps:t -> Fspath.t -> Path.local -> Unix.LargeFile.stats -> Osx.info -> t
val getWithRess : Unix.LargeFile.stats -> Osx.info -> basic
val check : Fspath.t -> Path.local -> Unix.LargeFile.stats -> x -> unit
val init : bool -> unit

val loadExtData : Fspath.t -> Path.local -> t -> x
val purgeExtData : x -> t
val withExtData : t -> x
(* [withExtData] will raise Not_found if some ext data is missing. In that
   case, [loadExtData] must be used to load any missing ext data. *)

val same_time : _ props -> t -> bool
val same_ctime : _ props -> t -> bool
val length : _ props -> Uutil.Filesize.t
val setLength : t -> Uutil.Filesize.t -> t
val time : _ props -> float
val setTime : t -> _ props -> t
val perms : _ props -> int

val fileDefault : basic
val fileSafe : t
val dirDefault : basic

val syncModtimes : bool Prefs.t
val permMask : int Prefs.t
val dontChmod : bool Prefs.t
val syncXattrs : bool Prefs.t

val xattrEnabled : unit -> bool

val xattrIgnorePred : Pred.t
val xattrIgnorenotPred : Pred.t

(* We are reusing the directory length to store a flag indicating that
   the directory is unchanged *)
type dirChangedStamp
val mdirChangedStamp : dirChangedStamp Umarshal.t
val freshDirStamp : unit -> dirChangedStamp
val changedDirStamp : dirChangedStamp
val setDirChangeFlag : t -> dirChangedStamp -> int -> t * bool
val dirMarkedUnchanged : t -> dirChangedStamp -> int -> bool

val validatePrefs: unit -> unit

module Data : sig
  type e
  type d = e list

  val m : d Umarshal.t

  val enabled : unit -> bool

  val extern : [`New] -> d
  val intern : d -> unit
  val merge : d -> unit

  val gcInit : unit -> unit
  val gcKeep : t -> unit
  val gcDone : unit -> d
end
