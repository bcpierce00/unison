(* Unison file synchronizer: src/fspath.mli *)
(* Copyright 1999-2014, Benjamin C. Pierce (see COPYING for details) *)

(* Defines an abstract type of absolute filenames (fspaths) *)

type t

val child : t -> Name.t -> t
val concat : t -> Path.local -> t

val canonize : string option -> t
val toString : t -> string
val toPrintString : t -> string
val toDebugString : t -> string
val toSysPath : t -> System.fspath

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

(* Escaped fspath (to pass as shell parameter) *)
val quotes : t -> string

(* CASE-SENSITIVE comparison between fspaths *)
val compare : t -> t -> int
