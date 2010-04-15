(* Unison file synchronizer: src/checksum.ml *)
(* Copyright 1999-2009, Benjamin C. Pierce 

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


(* The checksum (or fast fingerprinting) algorithm must be fast and has to   *)
(* be called in a rolling fashion (i.e. we must be able to calculate a new   *)
(* checksum when provided the current checksum, the outgoing character and   *)
(* the incoming one).                                                        *)

(* Definition: cksum([c_n, c_{n-1}, ..., c_0]) = Sum c_i * 16381 ^ i         *)

type t = int

type u = int array

(* [power v n] computes [v ^ n]                                              *)
let rec power v n =
  if n = 0 then 1 else
  let v' = power v (n / 2) in
  let v'' = v' * v' in
  if n land 1 <> 0 then v * v'' else v''

(* Takes the block length and returns a pre-computed table for the function  *)
(* roll: If [init l] => I, then I_n = n * 16381 ^ (l + 1), for 0 <= n < 256  *)
(* NB: 256 is the upper-bound of ASCII code returned by Char.code            *)

let init l = 
  let p = power 16381 (l + 1) in
  Array.init 256 (fun i -> (i * p) land 0x7fffffff)

(* Function [roll] computes fixed-length checksum from previous checksum     *)
(* Roughly: given the pre-computed table, compute the new checksum from the  *)
(* old one along with the outgoing and incoming characters, i.e.,            *)
(*                                                                         - *)
(* [roll cksum([c_n, ..., c_0]) c_n c'] => cksum([c_{n-1}, ..., c_0, c'])    *)
(*                                                                         - *)
let roll init cksum cout cin =
  let v = cksum + Char.code cin in
  (v lsl 14 - (v + v + v) (* v * 16381 *)
    - Array.unsafe_get init (Char.code cout)) land 0x7fffffff

(* Function [substring] computes checksum for a given substring in one batch *)
(* process: [substring s p l] => cksum([s_p, ..., s_{p + l - 1}])            *)

let substring s p l =
  let cksum = ref 0 in
  for i = p to p + l - 1 do
    let v = !cksum + Char.code (String.unsafe_get s i) in
    cksum := (v lsl 14 - (v + v + v)) (* v * 16381 *)
  done;
  !cksum land 0x7fffffff
