(* Unison file synchronizer: src/ubase/proplist.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

type 'a key
type t

val register : string -> 'a key

val empty : t
val mem : 'a key -> t -> bool
val find : 'a key -> t -> 'a
val add : 'a key -> 'a -> t -> t
