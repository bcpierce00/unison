(* Unison file synchronizer: src/path.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* Abstract type of relative pathnames *)
type 'a path

(* Pathname valid on both replicas (case insensitive in case
   insensitive mode) *)
type t = [`Global] path

(* Pathname specialized to a replica (case sensitive on a case
   sensitive filesystem) *)
type local = [`Local] path

val empty : 'a path
val length : t -> int
val isEmpty : local -> bool

val child : 'a path -> Name.t -> 'a path
val parent : local -> local
val finalName : t -> Name.t option
val deconstruct : t -> (Name.t * t) option
val deconstructRev : local -> (Name.t * local) option

val fromString : string -> 'a path
val toNames : t -> Name.t list
val toString : 'a path -> string
val toDebugString : local -> string

val addSuffixToFinalName : local -> string -> local
val addPrefixToFinalName : local -> string -> local

val compare : t -> t -> int
val hash : local -> int

val followLink : local -> bool

val magic : t -> local
val magic' : local -> t
