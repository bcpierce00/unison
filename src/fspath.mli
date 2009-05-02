(* Unison file synchronizer: src/fspath.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* Defines an abstract type of absolute filenames (fspaths)                  *)

type t

val child : t -> Name.t -> t
val concat : t -> Path.local -> t

val canonize : string option -> t
val toString : t -> string
val concatToString : t -> Path.local -> string

(* If fspath+path refers to a (followed) symlink, then return the directory  *)
(* of the symlink's target; otherwise return the parent dir of path.  If     *)
(* fspath+path is a root directory, raise Fatal.                             *)
val findWorkingDir : t -> Path.local -> (t * Path.local)

(* Return the least distinguishing suffixes of two fspaths, for displaying   *)
(* in the user interface.                                                    *)
val differentSuffix: t -> t -> (string * string)

(* Return the AppleDouble filename; if root dir, raise Invalid_argument      *)
val appleDouble : t -> t
(* Return the resource fork filename; if root dir, raise Invalid_argument    *)
val rsrc : t -> t

(* Wrapped system calls that use invariants of the fspath internal rep       *)
(* BE SURE TO USE ONLY THESE, NOT VERSIONS FROM THE UNIX MODULE!             *)
val stat : t -> Unix.LargeFile.stats
val lstat : t -> Unix.LargeFile.stats
val opendir : t -> Unix.dir_handle
