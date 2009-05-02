(* Unison file synchronizer: src/fileinfo.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

type typ = [`ABSENT | `FILE | `DIRECTORY | `SYMLINK]
val type2string : typ -> string

type t = { typ : typ; inode : int; ctime : float;
           desc : Props.t; osX : Osx.info}

val get : bool -> Fspath.t -> Path.local -> t
val set : Fspath.t -> Path.local ->
          [`Set of Props.t | `Copy of Path.local | `Update of Props.t] ->
          Props.t -> unit

(* IF THIS CHANGES, MAKE SURE TO INCREMENT THE ARCHIVE VERSION NUMBER!       *)
type stamp =
    InodeStamp of int         (* inode number, for Unix systems *)
  | CtimeStamp of float       (* creation time, for windows systems *)

val stamp : t -> stamp

val ressStamp : t -> Osx.ressStamp

(* Check whether a file is unchanged *)
val unchanged : Fspath.t -> Path.local -> t -> (t * bool * bool)
