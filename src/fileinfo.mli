(* Unison file synchronizer: src/fileinfo.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

type typ = [`ABSENT | `FILE | `DIRECTORY | `SYMLINK]
val mtyp : typ Umarshal.t
val type2string : typ -> string

type t251 = { typ : typ; inode : int; desc : Props.t251; osX : Osx.info}
type ('a, 'b) info = private { typ : typ; inode : int; desc : 'a; osX : Osx.info }
     constraint 'a = _ Props.props
type t = (Props.t, [`WithRess]) info
type basic = (Props.basic, [`NoRess]) info
type bress = (Props.basic, [`WithRess]) info

val basic : bress -> basic

val m : t Umarshal.t
val mbasic : basic Umarshal.t

val to_compat251 : basic -> t251
val of_compat251 : t251 -> basic

val getType : bool (* fromRoot *) -> Fspath.t -> Path.local -> typ
val getBasic : bool (* fromRoot *) -> Fspath.t -> Path.local -> basic
val getBasicWithRess : bool (* fromRoot *) -> Fspath.t -> Path.local -> bress
val get : bool (* fromRoot *) -> Fspath.t -> Path.local -> t
val set : Fspath.t -> Path.local ->
          [`Set of Props.basic | `Copy of Path.local | `Update of Props.t] ->
          Props.t -> unit

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

val stamp : _ info -> stamp

val ressStamp : t -> Osx.ressStamp

(* Check whether a file is unchanged *)
val unchanged : Fspath.t -> Path.local -> t -> (t * bool * bool)

(****)

val init : bool -> unit
val allowSymlinks : [`True|`False|`Default] Prefs.t
val ignoreInodeNumbers : bool Prefs.t
