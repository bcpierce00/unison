(* Unison file synchronizer: src/bytearray.mli *)
(* Copyright 1999-2010, Benjamin C. Pierce (see COPYING for details) *)

type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t

val length : t -> int

val to_string : t -> string

val of_string : string -> t

val sub : t -> int -> int -> string

val blit_from_string : string -> int -> t -> int -> int -> unit

val blit_to_string : t -> int -> string -> int -> int -> unit

val prefix : t -> t -> int -> bool

val marshal : 'a -> Marshal.extern_flags list -> t

val unmarshal : t -> int -> 'a
