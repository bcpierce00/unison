(* Unison file synchronizer: src/ubase/umarshal.mli *)
(* Copyright 2020, Stéphane Glondu (see COPYING for details) *)

exception Error of string

type 'a t

external id : 'a -> 'a = "%identity"

val header_size : int
val data_size : bytes -> int -> int
val to_string : 'a t -> 'a -> string
val from_bytes : 'a t -> bytes -> int -> 'a

val unit : unit t
val int : int t
val int64 : int64 t
val string : string t
val float : float t

val option : 'a t -> 'a option t
val list : 'a t -> 'a list t

val prod2 : 'a t -> 'b t -> ('r -> 'a * 'b) -> ('a * 'b -> 'r) -> 'r t
val prod3 : 'a t -> 'b t -> 'c t -> ('r -> 'a * 'b * 'c) -> ('a * 'b * 'c-> 'r) -> 'r t
val prod4 : 'a t -> 'b t -> 'c t -> 'd t -> ('r -> 'a * 'b * 'c * 'd) -> ('a * 'b * 'c * 'd -> 'r) -> 'r t
val prod6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('r -> 'a * 'b * 'c * 'd * 'e * 'f) -> ('a * 'b * 'c * 'd * 'e * 'f -> 'r) -> 'r t

type ('a, 'b) sum2 = I21 of 'a | I22 of 'b
val sum2 : 'a t -> 'b t -> ('r -> ('a, 'b) sum2) -> (('a, 'b) sum2 -> 'r) -> 'r t

type ('a, 'b, 'c) sum3 = I31 of 'a | I32 of 'b | I33 of 'c
val sum3 : 'a t -> 'b t -> 'c t -> ('r -> ('a, 'b, 'c) sum3) -> (('a, 'b, 'c) sum3 -> 'r) -> 'r t
