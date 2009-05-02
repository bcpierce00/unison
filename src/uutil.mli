(* Unison file synchronizer: src/uutil.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* This module collects a number of low-level, Unison-specific utility
   functions.  It is kept separate from the Util module so that that module
   can be re-used by other programs. *)

(* Identification *)
val myMajorVersion : string
val myVersion : string
val myName : string
val myNameAndVersion : string

(* Hashing *)
val hash2 : int -> int -> int

module type FILESIZE = sig
  type t
  val zero : t
  val dummy : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val toFloat : t -> float
  val toString : t -> string
  val ofInt : int -> t
  val ofInt64 : int64 -> t
  val toInt : t -> int
  val toInt64 : t -> int64
  val fromStats : Unix.LargeFile.stats -> t
  val hash : t -> int
  val percentageOfTotalSize : t -> t -> float
end

module Filesize : FILESIZE

(* The UI may (if it likes) supply a function to be used to show progress of *)
(* file transfers.                                                           *)
module File :
  sig
    type t
    val ofLine : int -> t
    val toLine : t -> int
    val toString : t -> string
    val dummy : t
  end
val setProgressPrinter :
  (File.t -> Filesize.t ->  string -> unit) -> unit
val showProgress : File.t -> Filesize.t -> string -> unit

(* Utility function to transfer bytes from one file descriptor to another
   until EOF *)
val readWrite :
     in_channel                 (* source *)
  -> out_channel                (* target *)
  -> (int -> unit)              (* progress notification *)
  -> unit

(* Utility function to transfer a given number of bytes from one file
   descriptor to another *)
val readWriteBounded :
     in_channel                 (* source *)
  -> out_channel                (* target *)
  -> Filesize.t
  -> (int -> unit)              (* progress notification *)
  -> unit
