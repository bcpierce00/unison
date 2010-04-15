(* Unison file synchronizer: src/ubase/safelist.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* All functions here are tail recursive and will work for arbitrary
   sized lists (unlike some of the standard ones).  The intention is that
   the built-in List module should not be referred to outside this module. *)

(* Functions from built-in List module *)
val map : ('a -> 'b) -> 'a list -> 'b list
val rev_map : ('a -> 'b) -> 'a list -> 'b list
val append : 'a list -> 'a list -> 'a list
val rev_append : 'a list -> 'a list -> 'a list
val concat : 'a list list -> 'a list
val combine : 'a list -> 'b list -> ('a * 'b) list
val iter : ('a -> unit) -> 'a list -> unit
val iteri : (int -> 'a -> unit) -> 'a list -> unit   (* zero-based *)
val rev : 'a list -> 'a list
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val nth : 'a list -> int -> 'a
val length : 'a list -> int
val mem : 'a -> 'a list -> bool
val flatten : 'a list list -> 'a list
val assoc : 'a -> ('a * 'b) list -> 'b
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val split : ('a * 'b) list -> 'a list * 'b list
val find : ('a -> bool) -> 'a list -> 'a
val filter : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
val sort : ('a -> 'a -> int) -> 'a list -> 'a list

(* Other useful list-processing functions *)
val filterMap : ('a -> 'b option) -> 'a list -> 'b list
val filterMap2 : ('a -> 'b option * 'c option) -> 'a list -> 'b list * 'c list
val transpose : 'a list list -> 'a list list
val filterBoth : ('a -> bool) -> 'a list -> ('a list * 'a list)
val allElementsEqual : 'a list -> bool
val flatten_map : ('a -> 'b list) -> 'a list -> 'b list
val remove : 'a -> 'a list -> 'a list
