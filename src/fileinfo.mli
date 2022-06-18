(* Unison file synchronizer: src/fileinfo.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

type typ = [`ABSENT | `FILE | `DIRECTORY | `SYMLINK]
val mtyp : typ Umarshal.t
val type2string : typ -> string

type t251 = { typ : typ; inode : int; desc : Props.t251; osX : Osx.info}
type t = { typ : typ; inode : int; desc : Props.t; osX : Osx.info}

val m : t Umarshal.t

val to_compat251 : t -> t251
val of_compat251 : t251 -> t

val get : bool (* fromRoot *) -> Fspath.t -> Path.local -> t
val set : Fspath.t -> Path.local ->
          [`Set of Props.t | `Copy of Path.local | `Update of Props.t] ->
          Props.t -> unit
val get' : string -> t

(* IF THIS CHANGES, MAKE SURE TO INCREMENT THE ARCHIVE VERSION NUMBER!       *)
type stamp251 =
    InodeStamp of int         (* inode number, for Unix systems *)
  | CtimeStamp of float       (* creation time, for windows systems *)

type stamp =
  | InodeStamp of int         (* inode number, for Unix systems *)
  | NoStamp
  | RescanStamp               (* stamp indicating file should be rescanned
                                 (perhaps because previous transfer failed) *)

val mstamp : stamp Umarshal.t

val stamp_to_compat251 : stamp -> stamp251
val stamp_of_compat251 : stamp251 -> stamp

val stamp : t -> stamp

val ressStamp : t -> Osx.ressStamp

(* Check whether a file is unchanged *)
val unchanged : Fspath.t -> Path.local -> t -> (t * bool * bool)

(****)

val init : bool -> unit
val allowSymlinks : [`True|`False|`Default] Prefs.t
val ignoreInodeNumbers : bool Prefs.t
