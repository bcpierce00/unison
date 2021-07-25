module Bytes = struct

include Bytes

(* The following code is taken from OCaml sources.
   Authors of the code snippet: Alain Frisch and Daniel Bünzli *)

(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {6 Binary encoding/decoding of integers} *)

external get_uint8 : bytes -> int -> int = "%bytes_safe_get"
external get_uint16_ne : bytes -> int -> int = "%caml_string_get16"
external get_int32_ne : bytes -> int -> int32 = "%caml_string_get32"
external get_int64_ne : bytes -> int -> int64 = "%caml_string_get64"
external set_int8 : bytes -> int -> int -> unit = "%bytes_safe_set"
external set_int16_ne : bytes -> int -> int -> unit = "%caml_string_set16"
external set_int32_ne : bytes -> int -> int32 -> unit = "%caml_string_set32"
external set_int64_ne : bytes -> int -> int64 -> unit = "%caml_string_set64"
external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"

let get_int8 b i =
  ((get_uint8 b i) lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let get_uint16_le b i =
  if Sys.big_endian then swap16 (get_uint16_ne b i)
  else get_uint16_ne b i

let get_uint16_be b i =
  if not Sys.big_endian then swap16 (get_uint16_ne b i)
  else get_uint16_ne b i

let get_int16_ne b i =
  ((get_uint16_ne b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_le b i =
  ((get_uint16_le b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_be b i =
  ((get_uint16_be b i) lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int32_le b i =
  if Sys.big_endian then swap32 (get_int32_ne b i)
  else get_int32_ne b i

let get_int32_be b i =
  if not Sys.big_endian then swap32 (get_int32_ne b i)
  else get_int32_ne b i

let get_int64_le b i =
  if Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

let get_int64_be b i =
  if not Sys.big_endian then swap64 (get_int64_ne b i)
  else get_int64_ne b i

let set_int16_le b i x =
  if Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int16_be b i x =
  if not Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int32_le b i x =
  if Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int32_be b i x =
  if not Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int64_le b i x =
  if Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

let set_int64_be b i x =
  if not Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

let set_uint8 = set_int8
let set_uint16_ne = set_int16_ne
let set_uint16_be = set_int16_be
let set_uint16_le = set_int16_le

end
