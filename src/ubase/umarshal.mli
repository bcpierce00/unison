(* Unison file synchronizer: src/ubase/umarshal.mli *)
(* Copyright 2020, Stéphane Glondu (see COPYING for details) *)

exception Error of string

type 'a t

external id : 'a -> 'a = "%identity"

val header_size : int
val data_size : bytes -> int -> int

val to_string : 'a t -> 'a -> string
val from_bytes : 'a t -> bytes -> int -> 'a

val rec1 : ('a t -> 'a t) -> 'a t
val rec2 : ('a t -> 'b t) -> ('b t -> 'a t) -> 'a t * 'b t

val unit : unit t
val bool : bool t
val int : int t
val int64 : int64 t
val float : float t

val string : string t

type bytearray =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val bytearray : bytearray t

val marshal_to_bytearray : 'a t -> 'a -> bytearray
val unmarshal_from_bytearray : 'a t -> bytearray -> int -> 'a

type int32bigarray =
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

val int32bigarray : int32bigarray t

val option : 'a t -> 'a option t
val list : 'a t -> 'a list t

val prod2 : 'a t -> 'b t -> ('r -> 'a * 'b) -> ('a * 'b -> 'r) -> 'r t
val prod3 : 'a t -> 'b t -> 'c t -> ('r -> 'a * 'b * 'c) -> ('a * 'b * 'c-> 'r) -> 'r t
val prod4 : 'a t -> 'b t -> 'c t -> 'd t -> ('r -> 'a * 'b * 'c * 'd) -> ('a * 'b * 'c * 'd -> 'r) -> 'r t
val prod5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('r -> 'a * 'b * 'c * 'd * 'e) -> ('a * 'b * 'c * 'd * 'e -> 'r) -> 'r t
val prod6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('r -> 'a * 'b * 'c * 'd * 'e * 'f) -> ('a * 'b * 'c * 'd * 'e * 'f -> 'r) -> 'r t

val sum1 : 'a t -> ('r -> 'a) -> ('a -> 'r) -> 'r t

type ('a, 'b) sum2 = I21 of 'a | I22 of 'b
val sum2 : 'a t -> 'b t -> ('r -> ('a, 'b) sum2) -> (('a, 'b) sum2 -> 'r) -> 'r t

type ('a, 'b, 'c) sum3 = I31 of 'a | I32 of 'b | I33 of 'c
val sum3 : 'a t -> 'b t -> 'c t -> ('r -> ('a, 'b, 'c) sum3) -> (('a, 'b, 'c) sum3 -> 'r) -> 'r t

type ('a, 'b, 'c, 'd) sum4 = I41 of 'a | I42 of 'b | I43 of 'c | I44 of 'd
val sum4 : 'a t -> 'b t -> 'c t -> 'd t -> ('r -> ('a, 'b, 'c, 'd) sum4) -> (('a, 'b, 'c, 'd) sum4 -> 'r) -> 'r t

type ('a, 'b, 'c, 'd, 'e) sum5 = I51 of 'a | I52 of 'b | I53 of 'c | I54 of 'd | I55 of 'e
val sum5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> ('r -> ('a, 'b, 'c, 'd, 'e) sum5) -> (('a, 'b, 'c, 'd, 'e) sum5 -> 'r) -> 'r t

type ('a, 'b, 'c, 'd, 'e, 'f) sum6 = I61 of 'a | I62 of 'b | I63 of 'c | I64 of 'd | I65 of 'e | I66 of 'f
val sum6 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t -> ('r -> ('a, 'b, 'c, 'd, 'e, 'f) sum6) -> (('a, 'b, 'c, 'd, 'e, 'f) sum6 -> 'r) -> 'r t

module type PROPLIST_S = sig
  type key = string
  type value = Obj.t
  type map
  val cardinal : map -> int
  val empty : map
  val add : key -> value -> map -> map
  val iter : (key -> value -> unit) -> map -> unit
  val find_m : key -> value t
end

module Proplist (S : PROPLIST_S) : sig
  val m : S.map t
end
