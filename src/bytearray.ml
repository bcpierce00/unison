(* Unison file synchronizer: src/bytearray.ml *)
(* Copyright 1999-2012, Benjamin C. Pierce 

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open Bigarray

type t = (char, int8_unsigned_elt, c_layout) Array1.t

let length = Bigarray.Array1.dim

let create l = Bigarray.Array1.create Bigarray.char Bigarray.c_layout l

(*
let unsafe_blit_from_string s i a j l =
  for k = 0 to l - 1 do
    a.{j + k} <- s.[i + k]
  done

let unsafe_blit_to_string a i s j l =
  for k = 0 to l - 1 do
    s.[j + k] <- a.{i + k}
  done
*)

external unsafe_blit_from_string : string -> int -> t -> int -> int -> unit
  = "ml_blit_string_to_bigarray" "noalloc"

external unsafe_blit_to_string : t -> int -> string -> int -> int -> unit
  = "ml_blit_bigarray_to_string" "noalloc"

let to_string a =
  let l = length a in
  if l > Sys.max_string_length then invalid_arg "Bytearray.to_string" else
  let s = String.create l in
  unsafe_blit_to_string a 0 s 0 l;
  s

let of_string s =
  let l = String.length s in
  let a = create l in
  unsafe_blit_from_string s 0 a 0 l;
  a

let sub a ofs len =
  if
    ofs < 0 || len < 0 || ofs > length a - len || len > Sys.max_string_length
  then
    invalid_arg "Bytearray.sub"
  else begin
    let s = String.create len in
    unsafe_blit_to_string a ofs s 0 len;
    s
  end

let rec prefix_rec a i a' i' l =
  l = 0 ||
  (a.{i} = a'.{i'} && prefix_rec a (i + 1) a' (i' + 1) (l - 1))

let prefix a a' i =
  let l = length a in
  let l' = length a' in
  i <= l' - l &&
  prefix_rec a 0 a' i l

let blit_from_string s i a j l =
  if l < 0 || i < 0 || i > String.length s - l
           || j < 0 || j > length a - l
  then invalid_arg "Bytearray.blit_from_string"
  else unsafe_blit_from_string s i a j l

let blit_to_string a i s j l =
  if l < 0 || i < 0 || i > length a - l
           || j < 0 || j > String.length s - l
  then invalid_arg "Bytearray.blit_to_string"
  else unsafe_blit_to_string a i s j l

external marshal : 'a -> Marshal.extern_flags list -> t
  = "ml_marshal_to_bigarray"

external unmarshal : t -> int -> 'a
  = "ml_unmarshal_from_bigarray"
