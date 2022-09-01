(* Unison file synchronizer: src/propsdata.mli *)
(* Copyright 2022, Tõivo Leedjärv (see COPYING for details) *)

module type S = sig
  val get : [< `All | `New | `Kept] -> (string * string) list
  val set : (string * string) list -> unit
  val merge : (string * string) list -> unit
  val clear : [`Kept] -> unit
end

module Xattr : sig
  include S

  val add : string -> string -> unit
  val find_opt : string -> string option
  val length : unit -> int
end

module ACL : sig
  include S

  val add : string -> string -> unit
  val find : string -> string
  val keep : string -> unit
end
