(* Unison file synchronizer: src/unicode.ml *)
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

open Unicode_tables

exception Invalid

let fail () = raise Invalid

let get s i = Char.code (String.unsafe_get s i)
let set s i v = String.unsafe_set s i (Char.unsafe_chr v)

(****)

let rec decode_char s i l =
  if i = l then fail () else
  let c = get s i in
  if c < 0x80 then
    cont s (i + 1) l c
  else if c < 0xE0 then begin
    (* 80 - 7FF *)
    if c < 0xc2 || i + 1 >= l then fail () else
    let c1 = get s (i + 1) in
    if c1 land 0xc0 <> 0x80 then fail () else
    let v = c lsl 6 + c1 - 0x3080 in
    cont s (i + 2) l v
  end else if c < 0xF0 then begin
    (* 800 - FFFF *)
    if i + 2 >= l then fail () else
    let c1 = get s (i + 1) in
    let c2 = get s (i + 2) in
    if (c1 lor c2) land 0xc0 <> 0x80 then fail () else
    let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
    if v < 0x800 then fail () else
    cont s (i + 3) l v
  end else begin
    (* 10000 - 10FFFF *)
    if i + 3 >= l then fail () else
    let c1 = get s (i + 1) in
    let c2 = get s (i + 2) in
    let c3 = get s (i + 3) in
    if (c1 lor c2 lor c3) land 0xc0 <> 0x80 then fail () else
    let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
    if v < 0x10000 || v > 0x10ffff then fail () else
    cont s (i + 4) l v
  end

and cont s i l v = (v, i)

let encode_char s i l c =
  if c < 0x80 then begin
    if i >= l then fail () else begin
      set s i c;
      i + 1
    end
  end else if c < 0x800 then begin
    if i + 1 >= l then fail () else begin
      set s i (c lsr 6 + 0xC0);
      set s (i + 1) (c land 0x3f + 0x80);
      i + 2
    end
  end else if c < 0x10000 then begin
    if i + 1 >= l then fail () else begin
      set s i (c lsr 12 + 0xE0);
      set s (i + 1) ((c lsr 6) land 0x3f + 0x80);
      set s (i + 2) (c land 0x3f + 0x80);
      i + 3
    end
  end else begin
    if i + 1 >= l then fail () else begin
      set s i (c lsr 18 + 0xF0);
      set s (i + 1) ((c lsr 12) land 0x3f + 0x80);
      set s (i + 2) ((c lsr 6) land 0x3f + 0x80);
      set s (i + 3) (c land 0x3f + 0x80);
      i + 4
    end
  end

let rec prev_char s i =
  let i = i - 1 in
  if i < 0 then fail () else
  if (get s i) land 0xc0 <> 0x80 then i else prev_char s i

(****)

let combining_property_bitmap = "\
\x00\x00\x00\x01\x02\x03\x04\x05\
\x00\x06\x07\x08\x09\x0A\x0B\x0C\
\x0D\x00\x00\x00\x00\x00\x00\x0E\
\x0F\x10\x00\x00\x00\x00\x00\x00\
\x11\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x12\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x13\x00\x00\x14\x00\
\xE6\xE6\xE6\xE6\xE6\xE6\xE6\xE6\
\xE6\xE6\xE6\xE6\xE6\xE6\xE6\xE6\
\xE6\xE6\xE6\xE6\xE6\xE8\xDC\xDC\
\xDC\xDC\xE8\xD8\xDC\xDC\xDC\xDC\
\xDC\xCA\xCA\xDC\xDC\xDC\xDC\xCA\
\xCA\xDC\xDC\xDC\xDC\xDC\xDC\xDC\
\xDC\xDC\xDC\xDC\x01\x01\x01\x01\
\x01\xDC\xDC\xDC\xDC\xE6\xE6\xE6\
\xE6\xE6\xE6\xE6\xE6\xF0\xE6\xDC\
\xDC\xDC\xE6\xE6\xE6\xDC\xDC\x00\
\xE6\xE6\xE6\xDC\xDC\xDC\xDC\xE6\
\x00\x00\x00\x00\x00\xEA\xEA\xE9\
\xEA\xEA\xE9\xE6\xE6\xE6\xE6\xE6\
\xE6\xE6\xE6\xE6\xE6\xE6\xE6\xE6\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\xE6\xE6\xE6\xE6\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\xDC\xE6\xE6\xE6\xE6\xDC\xE6\
\xE6\xE6\xDE\xDC\xE6\xE6\xE6\xE6\
\xE6\xE6\x00\xDC\xDC\xDC\xDC\xDC\
\xE6\xE6\xDC\xE6\xE6\xDE\xE4\xE6\
\x0A\x0B\x0C\x0D\x0E\x0F\x10\x11\
\x12\x13\x00\x14\x15\x16\x00\x17\
\x00\x18\x19\x00\xE6\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\xE6\xE6\xE6\xE6\xE6\xE6\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x1B\x1C\x1D\x1E\x1F\
\x20\x21\x22\xE6\xE6\xDC\xDC\xE6\
\xE6\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x23\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\xE6\xE6\
\xE6\xE6\xE6\xE6\xE6\x00\x00\xE6\
\xE6\xE6\xE6\xDC\xE6\x00\x00\xE6\
\xE6\x00\xDC\xE6\xE6\xDC\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x24\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\xE6\xDC\xE6\xE6\xDC\xE6\xE6\xDC\
\xDC\xDC\xE6\xDC\xDC\xE6\xDC\xE6\
\xE6\xE6\xDC\xE6\xDC\xE6\xDC\xE6\
\xDC\xE6\xE6\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x07\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\xE6\xDC\xE6\xE6\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x07\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x07\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x07\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x07\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x54\x5B\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x07\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x09\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x09\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x67\x67\x09\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x6B\x6B\x6B\x6B\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x76\x76\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x7A\x7A\x7A\x7A\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\xDC\xDC\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\xDC\x00\xDC\
\x00\xD8\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x81\x82\x00\x84\x00\x00\x00\
\x00\x00\x82\x82\x82\x82\x00\x00\
\x82\x00\xE6\xE6\x09\x00\xE6\xE6\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\xDC\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x07\
\x00\x09\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x09\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x09\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x09\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\xE6\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\xE4\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\xDE\xE6\xDC\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\xE6\xE6\x01\x01\xE6\xE6\xE6\xE6\
\x01\x01\x01\xE6\xE6\x00\x00\x00\
\x00\xE6\x00\x00\x00\x01\x01\xE6\
\xDC\xE6\x01\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\xDA\xE4\xE8\xDE\xE0\xE0\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x08\x08\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x1A\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\xE6\xE6\xE6\xE6\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00"

let combining_class c =
  if c > 0xffff then 0 else
  let v = get combining_property_bitmap (c lsr 8) in
  if v = 0 then 0 else
  get combining_property_bitmap (v lsl 8 + c land 0xff)

let rec find_loc s i l p =
  if i = 0 then i else
  let i' = prev_char s i in
  let (v, _) = decode_char s i' l in
  let p' = combining_class v in
  if p' <= p then i else
  find_loc s i' l p

let rec scan s i l p =
  if i < l then begin
    let c = get s i in
    if c < 0x80 then
      scan s (i + 1) l 0
    else if c < 0xE0 then begin
      (* 80 - 7FF *)
      if i + 1 >= l then fail () else
      let c1 = get s (i + 1) in
      let v = c lsl 6 + c1 - 0x3080 in
      cont s i l (i + 2) p v
    end else if c < 0xF0 then begin
      (* 800 - FFFF *)
      if i + 2 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
      cont s i l (i + 3) p v
    end else begin
      (* 10000 - 10FFFF *)
      if i + 3 >= l then fail () else
      scan s (i + 4) l 0
    end
  end

and cont s i l j p v =
  let p' = combining_class v in
  if p' = 0 || p <= p' then
    scan s j l p'
  else begin
    (* move char to the right location *)
    let k = find_loc s i l p' in
    let d = j - i in
    let s' = String.sub s i d in
    String.blit s k s (k + d) (i - k);
    String.blit s' 0 s k d;
    scan s j l p
  end

let order s =
  scan s 0 (String.length s) 0

(****)

let hangul_sbase = 0xAC00
let hangul_lbase = 0x1100
let hangul_vbase = 0x1161
let hangul_tbase = 0x11A7

let hangul_scount = 11172
let hangul_lcount = 19
let hangul_vcount = 21
let hangul_tcount = 28
let hangul_ncount = hangul_vcount * hangul_tcount

let set_char_3 s i c =
  set s i (c lsr 12 + 0xE0);
  set s (i + 1) ((c lsr 6) land 0x3f + 0x80);
  set s (i + 2) (c land 0x3f + 0x80)

let rec norm s i l s' j =
  if i < l then begin
    let c = get s i in
    if c < 0x80 then begin
      set s' j (get norm_ascii c);
      norm s (i + 1) l s' (j + 1)
    end else if c < 0xE0 then begin
      (* 80 - 7FF *)
      if c < 0xc2 || i + 1 >= l then raise Invalid;
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then raise Invalid;
      let idx = get norm_prim (c - 0xc0) in
      let idx = idx lsl 6 + c1 - 0x80 in
      let k = get norm_second_high idx in
      if k = 0 then begin
        set s' j c;
        set s' (j + 1) c1;
        norm s (i + 2) l s' (j + 2)
      end else begin
        let k = (k - 2) lsl 8 + get norm_second_low idx in
        let n = get norm_repl k in
        String.blit norm_repl (k + 1) s' j n;
        norm s (i + 2) l s' (j + n)
      end
    end else if c < 0xF0 then begin
      (* 800 - FFFF *)
      if i + 2 >= l then raise Invalid;
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then raise Invalid;
      let idx = c lsl 6 + c1 - 0x3880 in
      if idx < 0x20 then raise Invalid;
      let c2 = get s (i + 2) in
      if c2 land 0xc0 <> 0x80 then raise Invalid;
      let idx = get norm_prim idx in
      let idx = idx lsl 6 + c2 - 0x80 in
      let k = get norm_second_high idx in
      if k = 0 then begin
        set s' j c;
        set s' (j + 1) c1;
        set s' (j + 2) c2;
        norm s (i + 3) l s' (j + 3)
      end else if k = 1 then begin
        let v = c lsl 12 + c1 lsl 6 + c2 - (0x000E2080 + hangul_sbase) in
        if v >= hangul_scount then begin
          set s' j c;
          set s' (j + 1) c1;
          set s' (j + 2) c2;
          norm s (i + 3) l s' (j + 3)
        end else begin
          set_char_3 s' j (v / hangul_ncount + hangul_lbase);
          set_char_3 s' (j + 3)
            ((v mod hangul_ncount) / hangul_tcount + hangul_vbase);
          if v mod hangul_tcount = 0 then
            norm s (i + 3) l s' (j + 6)
          else begin
            set_char_3 s' (j + 6) ((v mod hangul_tcount) + hangul_tbase);
            norm s (i + 3) l s' (j + 9)
          end
        end
      end else begin
        let k = (k - 2) lsl 8 + get norm_second_low idx in
        let n = get norm_repl k in
        String.blit norm_repl (k + 1) s' j n;
        norm s (i + 3) l s' (j + n)
      end
    end else begin
      (* 10000 - 10FFFF *)
      if i + 3 >= l then raise Invalid;
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      let c3 = get s (i + 3) in
      if (c1 lor c2 lor c3) land 0xc0 <> 0x80 then raise Invalid;
      let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
      if v < 0x10000 || v > 0x10ffff then raise Invalid;
      set s' j c;
      set s' (j + 1) c1;
      set s' (j + 2) c2;
      set s' (j + 3) c3;
      norm s (i + 4) l s' (j + 4)
    end
  end else
    String.sub s' 0 j

let normalize s =
  let l = String.length s in
  let s' = String.create (3 * l) in
  try
    let s' = norm s 0 l s' 0 in order s'; s'
  with Invalid ->
    (* We need a comparison function which is coherent (transitive)
       also with non-unicode strings.  The optimization below assumes
       a case-insensitive comparison on ASCII characters, thus we
       translate the string to lowercase *)
    String.lowercase s

(****)

let rec decomp s i l s' j =
  if i < l then begin
    let c = get s i in
    if c < 0x80 then begin
      set s' j (get decomp_ascii c);
      decomp s (i + 1) l s' (j + 1)
    end else if c < 0xE0 then begin
      (* 80 - 7FF *)
      if c < 0xc2 || i + 1 >= l then raise Invalid;
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then raise Invalid;
      let idx = get decomp_prim (c - 0xc0) in
      let idx = idx lsl 6 + c1 - 0x80 in
      let k = get decomp_second_high idx in
      if k = 0 then begin
        set s' j c;
        set s' (j + 1) c1;
        decomp s (i + 2) l s' (j + 2)
      end else begin
        let k = (k - 2) lsl 8 + get decomp_second_low idx in
        let n = get decomp_repl k in
        String.blit decomp_repl (k + 1) s' j n;
        decomp s (i + 2) l s' (j + n)
      end
    end else if c < 0xF0 then begin
      (* 800 - FFFF *)
      if i + 2 >= l then raise Invalid;
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then raise Invalid;
      let idx = c lsl 6 + c1 - 0x3880 in
      if idx < 0x20 then raise Invalid;
      let c2 = get s (i + 2) in
      if c2 land 0xc0 <> 0x80 then raise Invalid;
      let idx = get decomp_prim idx in
      let idx = idx lsl 6 + c2 - 0x80 in
      let k = get decomp_second_high idx in
      if k = 0 then begin
        set s' j c;
        set s' (j + 1) c1;
        set s' (j + 2) c2;
        decomp s (i + 3) l s' (j + 3)
      end else if k = 1 then begin
        let v = c lsl 12 + c1 lsl 6 + c2 - (0x000E2080 + hangul_sbase) in
        if v >= hangul_scount then begin
          set s' j c;
          set s' (j + 1) c1;
          set s' (j + 2) c2;
          decomp s (i + 3) l s' (j + 3)
        end else begin
          set_char_3 s' j (v / hangul_ncount + hangul_lbase);
          set_char_3 s' (j + 3)
            ((v mod hangul_ncount) / hangul_tcount + hangul_vbase);
          if v mod hangul_tcount = 0 then
            decomp s (i + 3) l s' (j + 6)
          else begin
            set_char_3 s' (j + 6) ((v mod hangul_tcount) + hangul_tbase);
            decomp s (i + 3) l s' (j + 9)
          end
        end
      end else begin
        let k = (k - 2) lsl 8 + get decomp_second_low idx in
        let n = get decomp_repl k in
        String.blit decomp_repl (k + 1) s' j n;
        decomp s (i + 3) l s' (j + n)
      end
    end else begin
      (* 10000 - 10FFFF *)
      if i + 3 >= l then raise Invalid;
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      let c3 = get s (i + 3) in
      if (c1 lor c2 lor c3) land 0xc0 <> 0x80 then raise Invalid;
      let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
      if v < 0x10000 || v > 0x10ffff then raise Invalid;
      set s' j c;
      set s' (j + 1) c1;
      set s' (j + 2) c2;
      set s' (j + 3) c3;
      decomp s (i + 4) l s' (j + 4)
    end
  end else
    String.sub s' 0 j

let decompose s =
  let l = String.length s in
  let s' = String.create (3 * l) in
  try
    let s' = decomp s 0 l s' 0 in order s'; s'
  with Invalid ->
    s

(****)

let rec compare_rec s s' i l =
  if i = l then begin
    if l < String.length s then 1 else
    if l < String.length s' then -1 else
    0
  end else begin
    let c = get s i in
    let c' = get s' i in
    if c < 0x80 && c' < 0x80 then begin
      let v = compare (get norm_ascii c) (get norm_ascii c') in
      if v <> 0 then v else compare_rec s s' (i + 1) l
    end else
      compare (normalize s) (normalize s')
  end

let case_insensitive_compare s s' =
  compare_rec s s' 0 (min (String.length s) (String.length s'))

(****)

let rec compare_cs_rec s s' i l =
  if i = l then begin
    if l < String.length s then 1 else
    if l < String.length s' then -1 else
    0
  end else begin
    let c = get s i in
    let c' = get s' i in
    if c < 0x80 && c' < 0x80 then begin
      let v = compare c c' in
      if v <> 0 then v else compare_cs_rec s s' (i + 1) l
    end else
      compare s s'
  end

let case_sensitive_compare s s' =
  compare_rec s s' 0 (min (String.length s) (String.length s'))

(****)

let uniCharPrecompSourceTable = [|
	0x00000300; 0x00540000; 0x00000301; 0x00750054;
	0x00000302; 0x002000C9; 0x00000303; 0x001C00E9;
	0x00000304; 0x002C0105; 0x00000306; 0x00200131;
	0x00000307; 0x002E0151; 0x00000308; 0x0036017F;
	0x00000309; 0x001801B5; 0x0000030A; 0x000601CD;
	0x0000030B; 0x000601D3; 0x0000030C; 0x002501D9;
	0x0000030F; 0x000E01FE; 0x00000311; 0x000C020C;
	0x00000313; 0x000E0218; 0x00000314; 0x00100226;
	0x0000031B; 0x00040236; 0x00000323; 0x002A023A;
	0x00000324; 0x00020264; 0x00000325; 0x00020266;
	0x00000326; 0x00040268; 0x00000327; 0x0016026C;
	0x00000328; 0x000A0282; 0x0000032D; 0x000C028C;
	0x0000032E; 0x00020298; 0x00000330; 0x0006029A;
	0x00000331; 0x001102A0; 0x00000338; 0x002C02B1;
	0x00000342; 0x001D02DD; 0x00000345; 0x003F02FA;
	0x00000653; 0x00010339; 0x00000654; 0x0006033A;
	0x00000655; 0x00010340; 0x0000093C; 0x00030341;
	0x000009BE; 0x00010344; 0x000009D7; 0x00010345;
	0x00000B3E; 0x00010346; 0x00000B56; 0x00010347;
	0x00000B57; 0x00010348; 0x00000BBE; 0x00020349;
	0x00000BD7; 0x0002034B; 0x00000C56; 0x0001034D;
	0x00000CC2; 0x0001034E; 0x00000CD5; 0x0003034F;
	0x00000CD6; 0x00010352; 0x00000D3E; 0x00020353;
	0x00000D57; 0x00010355; 0x00000DCA; 0x00020356;
	0x00000DCF; 0x00010358; 0x00000DDF; 0x00010359;
	0x0000102E; 0x0001035A; 0x00003099; 0x0030035B;
	0x0000309A; 0x000A038B
|]

let uniCharBMPPrecompDestinationTable = [|
	0x0041; 0x00C0; 0x0045; 0x00C8; 0x0049; 0x00CC; 0x004E; 0x01F8;
	0x004F; 0x00D2; 0x0055; 0x00D9; 0x0057; 0x1E80; 0x0059; 0x1EF2;
	0x0061; 0x00E0; 0x0065; 0x00E8; 0x0069; 0x00EC; 0x006E; 0x01F9;
	0x006F; 0x00F2; 0x0075; 0x00F9; 0x0077; 0x1E81; 0x0079; 0x1EF3;
	0x00A8; 0x1FED; 0x00C2; 0x1EA6; 0x00CA; 0x1EC0; 0x00D4; 0x1ED2;
	0x00DC; 0x01DB; 0x00E2; 0x1EA7; 0x00EA; 0x1EC1; 0x00F4; 0x1ED3;
	0x00FC; 0x01DC; 0x0102; 0x1EB0; 0x0103; 0x1EB1; 0x0112; 0x1E14;
	0x0113; 0x1E15; 0x014C; 0x1E50; 0x014D; 0x1E51; 0x01A0; 0x1EDC;
	0x01A1; 0x1EDD; 0x01AF; 0x1EEA; 0x01B0; 0x1EEB; 0x0391; 0x1FBA;
	0x0395; 0x1FC8; 0x0397; 0x1FCA; 0x0399; 0x1FDA; 0x039F; 0x1FF8;
	0x03A5; 0x1FEA; 0x03A9; 0x1FFA; 0x03B1; 0x1F70; 0x03B5; 0x1F72;
	0x03B7; 0x1F74; 0x03B9; 0x1F76; 0x03BF; 0x1F78; 0x03C5; 0x1F7A;
	0x03C9; 0x1F7C; 0x03CA; 0x1FD2; 0x03CB; 0x1FE2; 0x0415; 0x0400;
	0x0418; 0x040D; 0x0435; 0x0450; 0x0438; 0x045D; 0x1F00; 0x1F02;
	0x1F01; 0x1F03; 0x1F08; 0x1F0A; 0x1F09; 0x1F0B; 0x1F10; 0x1F12;
	0x1F11; 0x1F13; 0x1F18; 0x1F1A; 0x1F19; 0x1F1B; 0x1F20; 0x1F22;
	0x1F21; 0x1F23; 0x1F28; 0x1F2A; 0x1F29; 0x1F2B; 0x1F30; 0x1F32;
	0x1F31; 0x1F33; 0x1F38; 0x1F3A; 0x1F39; 0x1F3B; 0x1F40; 0x1F42;
	0x1F41; 0x1F43; 0x1F48; 0x1F4A; 0x1F49; 0x1F4B; 0x1F50; 0x1F52;
	0x1F51; 0x1F53; 0x1F59; 0x1F5B; 0x1F60; 0x1F62; 0x1F61; 0x1F63;
	0x1F68; 0x1F6A; 0x1F69; 0x1F6B; 0x1FBF; 0x1FCD; 0x1FFE; 0x1FDD;
	0x0041; 0x00C1; 0x0043; 0x0106; 0x0045; 0x00C9; 0x0047; 0x01F4;
	0x0049; 0x00CD; 0x004B; 0x1E30; 0x004C; 0x0139; 0x004D; 0x1E3E;
	0x004E; 0x0143; 0x004F; 0x00D3; 0x0050; 0x1E54; 0x0052; 0x0154;
	0x0053; 0x015A; 0x0055; 0x00DA; 0x0057; 0x1E82; 0x0059; 0x00DD;
	0x005A; 0x0179; 0x0061; 0x00E1; 0x0063; 0x0107; 0x0065; 0x00E9;
	0x0067; 0x01F5; 0x0069; 0x00ED; 0x006B; 0x1E31; 0x006C; 0x013A;
	0x006D; 0x1E3F; 0x006E; 0x0144; 0x006F; 0x00F3; 0x0070; 0x1E55;
	0x0072; 0x0155; 0x0073; 0x015B; 0x0075; 0x00FA; 0x0077; 0x1E83;
	0x0079; 0x00FD; 0x007A; 0x017A; 0x00A8; 0x0385; 0x00C2; 0x1EA4;
	0x00C5; 0x01FA; 0x00C6; 0x01FC; 0x00C7; 0x1E08; 0x00CA; 0x1EBE;
	0x00CF; 0x1E2E; 0x00D4; 0x1ED0; 0x00D5; 0x1E4C; 0x00D8; 0x01FE;
	0x00DC; 0x01D7; 0x00E2; 0x1EA5; 0x00E5; 0x01FB; 0x00E6; 0x01FD;
	0x00E7; 0x1E09; 0x00EA; 0x1EBF; 0x00EF; 0x1E2F; 0x00F4; 0x1ED1;
	0x00F5; 0x1E4D; 0x00F8; 0x01FF; 0x00FC; 0x01D8; 0x0102; 0x1EAE;
	0x0103; 0x1EAF; 0x0112; 0x1E16; 0x0113; 0x1E17; 0x014C; 0x1E52;
	0x014D; 0x1E53; 0x0168; 0x1E78; 0x0169; 0x1E79; 0x01A0; 0x1EDA;
	0x01A1; 0x1EDB; 0x01AF; 0x1EE8; 0x01B0; 0x1EE9; 0x0391; 0x0386;
	0x0395; 0x0388; 0x0397; 0x0389; 0x0399; 0x038A; 0x039F; 0x038C;
	0x03A5; 0x038E; 0x03A9; 0x038F; 0x03B1; 0x03AC; 0x03B5; 0x03AD;
	0x03B7; 0x03AE; 0x03B9; 0x03AF; 0x03BF; 0x03CC; 0x03C5; 0x03CD;
	0x03C9; 0x03CE; 0x03CA; 0x0390; 0x03CB; 0x03B0; 0x03D2; 0x03D3;
	0x0413; 0x0403; 0x041A; 0x040C; 0x0433; 0x0453; 0x043A; 0x045C;
	0x1F00; 0x1F04; 0x1F01; 0x1F05; 0x1F08; 0x1F0C; 0x1F09; 0x1F0D;
	0x1F10; 0x1F14; 0x1F11; 0x1F15; 0x1F18; 0x1F1C; 0x1F19; 0x1F1D;
	0x1F20; 0x1F24; 0x1F21; 0x1F25; 0x1F28; 0x1F2C; 0x1F29; 0x1F2D;
	0x1F30; 0x1F34; 0x1F31; 0x1F35; 0x1F38; 0x1F3C; 0x1F39; 0x1F3D;
	0x1F40; 0x1F44; 0x1F41; 0x1F45; 0x1F48; 0x1F4C; 0x1F49; 0x1F4D;
	0x1F50; 0x1F54; 0x1F51; 0x1F55; 0x1F59; 0x1F5D; 0x1F60; 0x1F64;
	0x1F61; 0x1F65; 0x1F68; 0x1F6C; 0x1F69; 0x1F6D; 0x1FBF; 0x1FCE;
	0x1FFE; 0x1FDE; 0x0041; 0x00C2; 0x0043; 0x0108; 0x0045; 0x00CA;
	0x0047; 0x011C; 0x0048; 0x0124; 0x0049; 0x00CE; 0x004A; 0x0134;
	0x004F; 0x00D4; 0x0053; 0x015C; 0x0055; 0x00DB; 0x0057; 0x0174;
	0x0059; 0x0176; 0x005A; 0x1E90; 0x0061; 0x00E2; 0x0063; 0x0109;
	0x0065; 0x00EA; 0x0067; 0x011D; 0x0068; 0x0125; 0x0069; 0x00EE;
	0x006A; 0x0135; 0x006F; 0x00F4; 0x0073; 0x015D; 0x0075; 0x00FB;
	0x0077; 0x0175; 0x0079; 0x0177; 0x007A; 0x1E91; 0x1EA0; 0x1EAC;
	0x1EA1; 0x1EAD; 0x1EB8; 0x1EC6; 0x1EB9; 0x1EC7; 0x1ECC; 0x1ED8;
	0x1ECD; 0x1ED9; 0x0041; 0x00C3; 0x0045; 0x1EBC; 0x0049; 0x0128;
	0x004E; 0x00D1; 0x004F; 0x00D5; 0x0055; 0x0168; 0x0056; 0x1E7C;
	0x0059; 0x1EF8; 0x0061; 0x00E3; 0x0065; 0x1EBD; 0x0069; 0x0129;
	0x006E; 0x00F1; 0x006F; 0x00F5; 0x0075; 0x0169; 0x0076; 0x1E7D;
	0x0079; 0x1EF9; 0x00C2; 0x1EAA; 0x00CA; 0x1EC4; 0x00D4; 0x1ED6;
	0x00E2; 0x1EAB; 0x00EA; 0x1EC5; 0x00F4; 0x1ED7; 0x0102; 0x1EB4;
	0x0103; 0x1EB5; 0x01A0; 0x1EE0; 0x01A1; 0x1EE1; 0x01AF; 0x1EEE;
	0x01B0; 0x1EEF; 0x0041; 0x0100; 0x0045; 0x0112; 0x0047; 0x1E20;
	0x0049; 0x012A; 0x004F; 0x014C; 0x0055; 0x016A; 0x0059; 0x0232;
	0x0061; 0x0101; 0x0065; 0x0113; 0x0067; 0x1E21; 0x0069; 0x012B;
	0x006F; 0x014D; 0x0075; 0x016B; 0x0079; 0x0233; 0x00C4; 0x01DE;
	0x00C6; 0x01E2; 0x00D5; 0x022C; 0x00D6; 0x022A; 0x00DC; 0x01D5;
	0x00E4; 0x01DF; 0x00E6; 0x01E3; 0x00F5; 0x022D; 0x00F6; 0x022B;
	0x00FC; 0x01D6; 0x01EA; 0x01EC; 0x01EB; 0x01ED; 0x0226; 0x01E0;
	0x0227; 0x01E1; 0x022E; 0x0230; 0x022F; 0x0231; 0x0391; 0x1FB9;
	0x0399; 0x1FD9; 0x03A5; 0x1FE9; 0x03B1; 0x1FB1; 0x03B9; 0x1FD1;
	0x03C5; 0x1FE1; 0x0418; 0x04E2; 0x0423; 0x04EE; 0x0438; 0x04E3;
	0x0443; 0x04EF; 0x1E36; 0x1E38; 0x1E37; 0x1E39; 0x1E5A; 0x1E5C;
	0x1E5B; 0x1E5D; 0x0041; 0x0102; 0x0045; 0x0114; 0x0047; 0x011E;
	0x0049; 0x012C; 0x004F; 0x014E; 0x0055; 0x016C; 0x0061; 0x0103;
	0x0065; 0x0115; 0x0067; 0x011F; 0x0069; 0x012D; 0x006F; 0x014F;
	0x0075; 0x016D; 0x0228; 0x1E1C; 0x0229; 0x1E1D; 0x0391; 0x1FB8;
	0x0399; 0x1FD8; 0x03A5; 0x1FE8; 0x03B1; 0x1FB0; 0x03B9; 0x1FD0;
	0x03C5; 0x1FE0; 0x0410; 0x04D0; 0x0415; 0x04D6; 0x0416; 0x04C1;
	0x0418; 0x0419; 0x0423; 0x040E; 0x0430; 0x04D1; 0x0435; 0x04D7;
	0x0436; 0x04C2; 0x0438; 0x0439; 0x0443; 0x045E; 0x1EA0; 0x1EB6;
	0x1EA1; 0x1EB7; 0x0041; 0x0226; 0x0042; 0x1E02; 0x0043; 0x010A;
	0x0044; 0x1E0A; 0x0045; 0x0116; 0x0046; 0x1E1E; 0x0047; 0x0120;
	0x0048; 0x1E22; 0x0049; 0x0130; 0x004D; 0x1E40; 0x004E; 0x1E44;
	0x004F; 0x022E; 0x0050; 0x1E56; 0x0052; 0x1E58; 0x0053; 0x1E60;
	0x0054; 0x1E6A; 0x0057; 0x1E86; 0x0058; 0x1E8A; 0x0059; 0x1E8E;
	0x005A; 0x017B; 0x0061; 0x0227; 0x0062; 0x1E03; 0x0063; 0x010B;
	0x0064; 0x1E0B; 0x0065; 0x0117; 0x0066; 0x1E1F; 0x0067; 0x0121;
	0x0068; 0x1E23; 0x006D; 0x1E41; 0x006E; 0x1E45; 0x006F; 0x022F;
	0x0070; 0x1E57; 0x0072; 0x1E59; 0x0073; 0x1E61; 0x0074; 0x1E6B;
	0x0077; 0x1E87; 0x0078; 0x1E8B; 0x0079; 0x1E8F; 0x007A; 0x017C;
	0x015A; 0x1E64; 0x015B; 0x1E65; 0x0160; 0x1E66; 0x0161; 0x1E67;
	0x017F; 0x1E9B; 0x1E62; 0x1E68; 0x1E63; 0x1E69; 0x0041; 0x00C4;
	0x0045; 0x00CB; 0x0048; 0x1E26; 0x0049; 0x00CF; 0x004F; 0x00D6;
	0x0055; 0x00DC; 0x0057; 0x1E84; 0x0058; 0x1E8C; 0x0059; 0x0178;
	0x0061; 0x00E4; 0x0065; 0x00EB; 0x0068; 0x1E27; 0x0069; 0x00EF;
	0x006F; 0x00F6; 0x0074; 0x1E97; 0x0075; 0x00FC; 0x0077; 0x1E85;
	0x0078; 0x1E8D; 0x0079; 0x00FF; 0x00D5; 0x1E4E; 0x00F5; 0x1E4F;
	0x016A; 0x1E7A; 0x016B; 0x1E7B; 0x0399; 0x03AA; 0x03A5; 0x03AB;
	0x03B9; 0x03CA; 0x03C5; 0x03CB; 0x03D2; 0x03D4; 0x0406; 0x0407;
	0x0410; 0x04D2; 0x0415; 0x0401; 0x0416; 0x04DC; 0x0417; 0x04DE;
	0x0418; 0x04E4; 0x041E; 0x04E6; 0x0423; 0x04F0; 0x0427; 0x04F4;
	0x042B; 0x04F8; 0x042D; 0x04EC; 0x0430; 0x04D3; 0x0435; 0x0451;
	0x0436; 0x04DD; 0x0437; 0x04DF; 0x0438; 0x04E5; 0x043E; 0x04E7;
	0x0443; 0x04F1; 0x0447; 0x04F5; 0x044B; 0x04F9; 0x044D; 0x04ED;
	0x0456; 0x0457; 0x04D8; 0x04DA; 0x04D9; 0x04DB; 0x04E8; 0x04EA;
	0x04E9; 0x04EB; 0x0041; 0x1EA2; 0x0045; 0x1EBA; 0x0049; 0x1EC8;
	0x004F; 0x1ECE; 0x0055; 0x1EE6; 0x0059; 0x1EF6; 0x0061; 0x1EA3;
	0x0065; 0x1EBB; 0x0069; 0x1EC9; 0x006F; 0x1ECF; 0x0075; 0x1EE7;
	0x0079; 0x1EF7; 0x00C2; 0x1EA8; 0x00CA; 0x1EC2; 0x00D4; 0x1ED4;
	0x00E2; 0x1EA9; 0x00EA; 0x1EC3; 0x00F4; 0x1ED5; 0x0102; 0x1EB2;
	0x0103; 0x1EB3; 0x01A0; 0x1EDE; 0x01A1; 0x1EDF; 0x01AF; 0x1EEC;
	0x01B0; 0x1EED; 0x0041; 0x00C5; 0x0055; 0x016E; 0x0061; 0x00E5;
	0x0075; 0x016F; 0x0077; 0x1E98; 0x0079; 0x1E99; 0x004F; 0x0150;
	0x0055; 0x0170; 0x006F; 0x0151; 0x0075; 0x0171; 0x0423; 0x04F2;
	0x0443; 0x04F3; 0x0041; 0x01CD; 0x0043; 0x010C; 0x0044; 0x010E;
	0x0045; 0x011A; 0x0047; 0x01E6; 0x0048; 0x021E; 0x0049; 0x01CF;
	0x004B; 0x01E8; 0x004C; 0x013D; 0x004E; 0x0147; 0x004F; 0x01D1;
	0x0052; 0x0158; 0x0053; 0x0160; 0x0054; 0x0164; 0x0055; 0x01D3;
	0x005A; 0x017D; 0x0061; 0x01CE; 0x0063; 0x010D; 0x0064; 0x010F;
	0x0065; 0x011B; 0x0067; 0x01E7; 0x0068; 0x021F; 0x0069; 0x01D0;
	0x006A; 0x01F0; 0x006B; 0x01E9; 0x006C; 0x013E; 0x006E; 0x0148;
	0x006F; 0x01D2; 0x0072; 0x0159; 0x0073; 0x0161; 0x0074; 0x0165;
	0x0075; 0x01D4; 0x007A; 0x017E; 0x00DC; 0x01D9; 0x00FC; 0x01DA;
	0x01B7; 0x01EE; 0x0292; 0x01EF; 0x0041; 0x0200; 0x0045; 0x0204;
	0x0049; 0x0208; 0x004F; 0x020C; 0x0052; 0x0210; 0x0055; 0x0214;
	0x0061; 0x0201; 0x0065; 0x0205; 0x0069; 0x0209; 0x006F; 0x020D;
	0x0072; 0x0211; 0x0075; 0x0215; 0x0474; 0x0476; 0x0475; 0x0477;
	0x0041; 0x0202; 0x0045; 0x0206; 0x0049; 0x020A; 0x004F; 0x020E;
	0x0052; 0x0212; 0x0055; 0x0216; 0x0061; 0x0203; 0x0065; 0x0207;
	0x0069; 0x020B; 0x006F; 0x020F; 0x0072; 0x0213; 0x0075; 0x0217;
	0x0391; 0x1F08; 0x0395; 0x1F18; 0x0397; 0x1F28; 0x0399; 0x1F38;
	0x039F; 0x1F48; 0x03A9; 0x1F68; 0x03B1; 0x1F00; 0x03B5; 0x1F10;
	0x03B7; 0x1F20; 0x03B9; 0x1F30; 0x03BF; 0x1F40; 0x03C1; 0x1FE4;
	0x03C5; 0x1F50; 0x03C9; 0x1F60; 0x0391; 0x1F09; 0x0395; 0x1F19;
	0x0397; 0x1F29; 0x0399; 0x1F39; 0x039F; 0x1F49; 0x03A1; 0x1FEC;
	0x03A5; 0x1F59; 0x03A9; 0x1F69; 0x03B1; 0x1F01; 0x03B5; 0x1F11;
	0x03B7; 0x1F21; 0x03B9; 0x1F31; 0x03BF; 0x1F41; 0x03C1; 0x1FE5;
	0x03C5; 0x1F51; 0x03C9; 0x1F61; 0x004F; 0x01A0; 0x0055; 0x01AF;
	0x006F; 0x01A1; 0x0075; 0x01B0; 0x0041; 0x1EA0; 0x0042; 0x1E04;
	0x0044; 0x1E0C; 0x0045; 0x1EB8; 0x0048; 0x1E24; 0x0049; 0x1ECA;
	0x004B; 0x1E32; 0x004C; 0x1E36; 0x004D; 0x1E42; 0x004E; 0x1E46;
	0x004F; 0x1ECC; 0x0052; 0x1E5A; 0x0053; 0x1E62; 0x0054; 0x1E6C;
	0x0055; 0x1EE4; 0x0056; 0x1E7E; 0x0057; 0x1E88; 0x0059; 0x1EF4;
	0x005A; 0x1E92; 0x0061; 0x1EA1; 0x0062; 0x1E05; 0x0064; 0x1E0D;
	0x0065; 0x1EB9; 0x0068; 0x1E25; 0x0069; 0x1ECB; 0x006B; 0x1E33;
	0x006C; 0x1E37; 0x006D; 0x1E43; 0x006E; 0x1E47; 0x006F; 0x1ECD;
	0x0072; 0x1E5B; 0x0073; 0x1E63; 0x0074; 0x1E6D; 0x0075; 0x1EE5;
	0x0076; 0x1E7F; 0x0077; 0x1E89; 0x0079; 0x1EF5; 0x007A; 0x1E93;
	0x01A0; 0x1EE2; 0x01A1; 0x1EE3; 0x01AF; 0x1EF0; 0x01B0; 0x1EF1;
	0x0055; 0x1E72; 0x0075; 0x1E73; 0x0041; 0x1E00; 0x0061; 0x1E01;
	0x0053; 0x0218; 0x0054; 0x021A; 0x0073; 0x0219; 0x0074; 0x021B;
	0x0043; 0x00C7; 0x0044; 0x1E10; 0x0045; 0x0228; 0x0047; 0x0122;
	0x0048; 0x1E28; 0x004B; 0x0136; 0x004C; 0x013B; 0x004E; 0x0145;
	0x0052; 0x0156; 0x0053; 0x015E; 0x0054; 0x0162; 0x0063; 0x00E7;
	0x0064; 0x1E11; 0x0065; 0x0229; 0x0067; 0x0123; 0x0068; 0x1E29;
	0x006B; 0x0137; 0x006C; 0x013C; 0x006E; 0x0146; 0x0072; 0x0157;
	0x0073; 0x015F; 0x0074; 0x0163; 0x0041; 0x0104; 0x0045; 0x0118;
	0x0049; 0x012E; 0x004F; 0x01EA; 0x0055; 0x0172; 0x0061; 0x0105;
	0x0065; 0x0119; 0x0069; 0x012F; 0x006F; 0x01EB; 0x0075; 0x0173;
	0x0044; 0x1E12; 0x0045; 0x1E18; 0x004C; 0x1E3C; 0x004E; 0x1E4A;
	0x0054; 0x1E70; 0x0055; 0x1E76; 0x0064; 0x1E13; 0x0065; 0x1E19;
	0x006C; 0x1E3D; 0x006E; 0x1E4B; 0x0074; 0x1E71; 0x0075; 0x1E77;
	0x0048; 0x1E2A; 0x0068; 0x1E2B; 0x0045; 0x1E1A; 0x0049; 0x1E2C;
	0x0055; 0x1E74; 0x0065; 0x1E1B; 0x0069; 0x1E2D; 0x0075; 0x1E75;
	0x0042; 0x1E06; 0x0044; 0x1E0E; 0x004B; 0x1E34; 0x004C; 0x1E3A;
	0x004E; 0x1E48; 0x0052; 0x1E5E; 0x0054; 0x1E6E; 0x005A; 0x1E94;
	0x0062; 0x1E07; 0x0064; 0x1E0F; 0x0068; 0x1E96; 0x006B; 0x1E35;
	0x006C; 0x1E3B; 0x006E; 0x1E49; 0x0072; 0x1E5F; 0x0074; 0x1E6F;
	0x007A; 0x1E95; 0x003C; 0x226E; 0x003D; 0x2260; 0x003E; 0x226F;
	0x2190; 0x219A; 0x2192; 0x219B; 0x2194; 0x21AE; 0x21D0; 0x21CD;
	0x21D2; 0x21CF; 0x21D4; 0x21CE; 0x2203; 0x2204; 0x2208; 0x2209;
	0x220B; 0x220C; 0x2223; 0x2224; 0x2225; 0x2226; 0x223C; 0x2241;
	0x2243; 0x2244; 0x2245; 0x2247; 0x2248; 0x2249; 0x224D; 0x226D;
	0x2261; 0x2262; 0x2264; 0x2270; 0x2265; 0x2271; 0x2272; 0x2274;
	0x2273; 0x2275; 0x2276; 0x2278; 0x2277; 0x2279; 0x227A; 0x2280;
	0x227B; 0x2281; 0x227C; 0x22E0; 0x227D; 0x22E1; 0x2282; 0x2284;
	0x2283; 0x2285; 0x2286; 0x2288; 0x2287; 0x2289; 0x2291; 0x22E2;
	0x2292; 0x22E3; 0x22A2; 0x22AC; 0x22A8; 0x22AD; 0x22A9; 0x22AE;
	0x22AB; 0x22AF; 0x22B2; 0x22EA; 0x22B3; 0x22EB; 0x22B4; 0x22EC;
	0x22B5; 0x22ED; 0x00A8; 0x1FC1; 0x03B1; 0x1FB6; 0x03B7; 0x1FC6;
	0x03B9; 0x1FD6; 0x03C5; 0x1FE6; 0x03C9; 0x1FF6; 0x03CA; 0x1FD7;
	0x03CB; 0x1FE7; 0x1F00; 0x1F06; 0x1F01; 0x1F07; 0x1F08; 0x1F0E;
	0x1F09; 0x1F0F; 0x1F20; 0x1F26; 0x1F21; 0x1F27; 0x1F28; 0x1F2E;
	0x1F29; 0x1F2F; 0x1F30; 0x1F36; 0x1F31; 0x1F37; 0x1F38; 0x1F3E;
	0x1F39; 0x1F3F; 0x1F50; 0x1F56; 0x1F51; 0x1F57; 0x1F59; 0x1F5F;
	0x1F60; 0x1F66; 0x1F61; 0x1F67; 0x1F68; 0x1F6E; 0x1F69; 0x1F6F;
	0x1FBF; 0x1FCF; 0x1FFE; 0x1FDF; 0x0391; 0x1FBC; 0x0397; 0x1FCC;
	0x03A9; 0x1FFC; 0x03AC; 0x1FB4; 0x03AE; 0x1FC4; 0x03B1; 0x1FB3;
	0x03B7; 0x1FC3; 0x03C9; 0x1FF3; 0x03CE; 0x1FF4; 0x1F00; 0x1F80;
	0x1F01; 0x1F81; 0x1F02; 0x1F82; 0x1F03; 0x1F83; 0x1F04; 0x1F84;
	0x1F05; 0x1F85; 0x1F06; 0x1F86; 0x1F07; 0x1F87; 0x1F08; 0x1F88;
	0x1F09; 0x1F89; 0x1F0A; 0x1F8A; 0x1F0B; 0x1F8B; 0x1F0C; 0x1F8C;
	0x1F0D; 0x1F8D; 0x1F0E; 0x1F8E; 0x1F0F; 0x1F8F; 0x1F20; 0x1F90;
	0x1F21; 0x1F91; 0x1F22; 0x1F92; 0x1F23; 0x1F93; 0x1F24; 0x1F94;
	0x1F25; 0x1F95; 0x1F26; 0x1F96; 0x1F27; 0x1F97; 0x1F28; 0x1F98;
	0x1F29; 0x1F99; 0x1F2A; 0x1F9A; 0x1F2B; 0x1F9B; 0x1F2C; 0x1F9C;
	0x1F2D; 0x1F9D; 0x1F2E; 0x1F9E; 0x1F2F; 0x1F9F; 0x1F60; 0x1FA0;
	0x1F61; 0x1FA1; 0x1F62; 0x1FA2; 0x1F63; 0x1FA3; 0x1F64; 0x1FA4;
	0x1F65; 0x1FA5; 0x1F66; 0x1FA6; 0x1F67; 0x1FA7; 0x1F68; 0x1FA8;
	0x1F69; 0x1FA9; 0x1F6A; 0x1FAA; 0x1F6B; 0x1FAB; 0x1F6C; 0x1FAC;
	0x1F6D; 0x1FAD; 0x1F6E; 0x1FAE; 0x1F6F; 0x1FAF; 0x1F70; 0x1FB2;
	0x1F74; 0x1FC2; 0x1F7C; 0x1FF2; 0x1FB6; 0x1FB7; 0x1FC6; 0x1FC7;
	0x1FF6; 0x1FF7; 0x0627; 0x0622; 0x0627; 0x0623; 0x0648; 0x0624;
	0x064A; 0x0626; 0x06C1; 0x06C2; 0x06D2; 0x06D3; 0x06D5; 0x06C0;
	0x0627; 0x0625; 0x0928; 0x0929; 0x0930; 0x0931; 0x0933; 0x0934;
	0x09C7; 0x09CB; 0x09C7; 0x09CC; 0x0B47; 0x0B4B; 0x0B47; 0x0B48;
	0x0B47; 0x0B4C; 0x0BC6; 0x0BCA; 0x0BC7; 0x0BCB; 0x0B92; 0x0B94;
	0x0BC6; 0x0BCC; 0x0C46; 0x0C48; 0x0CC6; 0x0CCA; 0x0CBF; 0x0CC0;
	0x0CC6; 0x0CC7; 0x0CCA; 0x0CCB; 0x0CC6; 0x0CC8; 0x0D46; 0x0D4A;
	0x0D47; 0x0D4B; 0x0D46; 0x0D4C; 0x0DD9; 0x0DDA; 0x0DDC; 0x0DDD;
	0x0DD9; 0x0DDC; 0x0DD9; 0x0DDE; 0x1025; 0x1026; 0x3046; 0x3094;
	0x304B; 0x304C; 0x304D; 0x304E; 0x304F; 0x3050; 0x3051; 0x3052;
	0x3053; 0x3054; 0x3055; 0x3056; 0x3057; 0x3058; 0x3059; 0x305A;
	0x305B; 0x305C; 0x305D; 0x305E; 0x305F; 0x3060; 0x3061; 0x3062;
	0x3064; 0x3065; 0x3066; 0x3067; 0x3068; 0x3069; 0x306F; 0x3070;
	0x3072; 0x3073; 0x3075; 0x3076; 0x3078; 0x3079; 0x307B; 0x307C;
	0x309D; 0x309E; 0x30A6; 0x30F4; 0x30AB; 0x30AC; 0x30AD; 0x30AE;
	0x30AF; 0x30B0; 0x30B1; 0x30B2; 0x30B3; 0x30B4; 0x30B5; 0x30B6;
	0x30B7; 0x30B8; 0x30B9; 0x30BA; 0x30BB; 0x30BC; 0x30BD; 0x30BE;
	0x30BF; 0x30C0; 0x30C1; 0x30C2; 0x30C4; 0x30C5; 0x30C6; 0x30C7;
	0x30C8; 0x30C9; 0x30CF; 0x30D0; 0x30D2; 0x30D3; 0x30D5; 0x30D6;
	0x30D8; 0x30D9; 0x30DB; 0x30DC; 0x30EF; 0x30F7; 0x30F0; 0x30F8;
	0x30F1; 0x30F9; 0x30F2; 0x30FA; 0x30FD; 0x30FE; 0x306F; 0x3071;
	0x3072; 0x3074; 0x3075; 0x3077; 0x3078; 0x307A; 0x307B; 0x307D;
	0x30CF; 0x30D1; 0x30D2; 0x30D4; 0x30D5; 0x30D7; 0x30D8; 0x30DA;
	0x30DB; 0x30DD
|]

let uniCharCombiningBitmap = "\
\x00\x00\x00\x01\x02\x03\x04\x05\
\x00\x06\x07\x08\x09\x0A\x0B\x0C\
\x0D\x14\x00\x00\x00\x00\x00\x0E\
\x0F\x00\x00\x00\x00\x00\x00\x00\
\x10\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x11\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x12\x00\x00\x13\x00\
\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\
\xFF\xFF\x00\x00\xFF\xFF\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x78\x03\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\xFE\xFF\xFB\xFF\xFF\xBB\
\x16\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\xF8\x3F\x00\x00\x00\x01\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\xC0\xFF\x9F\x3D\x00\x00\
\x00\x00\x02\x00\x00\x00\xFF\xFF\
\xFF\x07\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\xC0\xFF\x01\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x0E\x00\x00\x00\x00\x00\x00\xD0\
\xFF\x3F\x1E\x00\x0C\x00\x00\x00\
\x0E\x00\x00\x00\x00\x00\x00\xD0\
\x9F\x39\x80\x00\x0C\x00\x00\x00\
\x04\x00\x00\x00\x00\x00\x00\xD0\
\x87\x39\x00\x00\x00\x00\x03\x00\
\x0E\x00\x00\x00\x00\x00\x00\xD0\
\xBF\x3B\x00\x00\x00\x00\x00\x00\
\x0E\x00\x00\x00\x00\x00\x00\xD0\
\x8F\x39\xC0\x00\x00\x00\x00\x00\
\x04\x00\x00\x00\x00\x00\x00\xC0\
\xC7\x3D\x80\x00\x00\x00\x00\x00\
\x0E\x00\x00\x00\x00\x00\x00\xC0\
\xDF\x3D\x60\x00\x00\x00\x00\x00\
\x0C\x00\x00\x00\x00\x00\x00\xC0\
\xDF\x3D\x60\x00\x00\x00\x00\x00\
\x0C\x00\x00\x00\x00\x00\x00\xC0\
\xCF\x3D\x80\x00\x00\x00\x00\x00\
\x0C\x00\x00\x00\x00\x00\x00\x00\
\x00\x84\x5F\xFF\x00\x00\x0C\x00\
\x00\x00\x00\x00\x00\x00\xF2\x07\
\x80\x7F\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\xF2\x1B\
\x00\x3F\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x03\x00\x00\xA0\xC2\
\x00\x00\x00\x00\x00\x00\xFE\xFF\
\xDF\x00\xFF\xFE\xFF\xFF\xFF\x1F\
\x40\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\xF0\xC7\x03\
\x00\x00\xC0\x03\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x1C\x00\x00\x00\x1C\x00\
\x00\x00\x0C\x00\x00\x00\x0C\x00\
\x00\x00\x00\x00\x00\x00\xF0\xFF\
\xFF\xFF\x0F\x00\x00\x00\x00\x00\
\x00\x38\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x02\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\xFF\xFF\xFF\x07\x00\x00\
\x00\x00\x00\x00\x00\xFC\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x06\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x40\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\xFF\xFF\x00\x00\x0F\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\x00\x00\x00\x00\
\x00\x00\x00\x00\xFE\xFF\x3F\x00\
\x00\x00\x00\x00\x00\xFF\xFF\xFF\
\x07\x00\x00\x00\x00\x00\x00\x00"

(****)

let bitmap_test base bitmap character =
  character >= base && character < 0x10000
    &&
  (let value = get bitmap ((character lsr 8) land 0xFF) in
   value = 0xFF
      ||
   (value <> 0
       &&
    get bitmap ((value - 1) * 32 + 256 + (character land 0xFF) / 8)
      land (1 lsl (character land 7)) <> 0))

let unicode_combinable character =
  bitmap_test 0x0300 uniCharCombiningBitmap character

let rec find_rec t i j v =
  if i + 1 = j then begin
    if t.(i * 2) = v then t.(i * 2 + 1) else 0
  end else begin
    let k = (i + j) / 2 in
    if v < t.(k * 2) then
      find_rec t i k v
    else
      find_rec t k j v
  end

let find t i n v =
  let j = i + n in
  if v < t.(2 * i) || v > t.(2 * (j - 1)) then 0 else
  find_rec t i j v

let uniCharPrecompSourceTableLen = Array.length uniCharPrecompSourceTable / 2

let combine v v' =
  if v' >= hangul_vbase && v' < hangul_tbase + hangul_tcount then begin
    if
      v' < hangul_vbase + hangul_vcount &&
      v >= hangul_lbase && v < hangul_lbase + hangul_lcount
    then
      hangul_sbase + ((v - hangul_lbase) * (hangul_vcount * hangul_tcount)) +
                     ((v' - hangul_vbase) * hangul_tcount)
    else if
      v' > hangul_tbase &&
      v >= hangul_sbase && v < hangul_sbase + hangul_scount
    then
      if (v - hangul_sbase) mod hangul_tcount <> 0 then 0 else
      v + v' - hangul_tbase
    else
      0
  end else begin
    let k =
      find uniCharPrecompSourceTable 0
        uniCharPrecompSourceTableLen v'
    in
    if k = 0 then 0 else
    find uniCharBMPPrecompDestinationTable (k land 0xFFFF) (k lsr 16) v
  end

(****)

let rec scan d s i l =
  if i < l then begin
    let c = get s i in
    if c < 0x80 then
      cont d s i l (i + 1) c
    else if c < 0xE0 then begin
      (* 80 - 7FF *)
      if c < 0xc2 || i + 1 >= l then fail () else
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then fail () else
      let v = c lsl 6 + c1 - 0x3080 in
      cont d s i l (i + 2) v
    end else if c < 0xF0 then begin
      (* 800 - FFFF *)
      if i + 2 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      if (c1 lor c2) land 0xc0 <> 0x80 then fail () else
      let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
      if v < 0x800 then fail () else
      cont d s i l (i + 3) v
    end else begin
      (* 10000 - 10FFFF *)
      if i + 3 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      let c3 = get s (i + 3) in
      if (c1 lor c2 lor c3) land 0xc0 <> 0x80 then fail () else
      let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
      if v < 0x10000 || v > 0x10ffff then fail () else
      cont d s i l (i + 4) v
    end
  end else begin
    let (i1, i2) = d in
    String.blit s i2 s i1 (l - i2);
    String.sub s 0 (i1 + l - i2)
  end

and cont d s i l j v' =
  if unicode_combinable v' then begin
    let i = prev_char s i in
    let (v, _) = decode_char s i l in
    let v'' = combine v v' in
    if v'' = 0 then
      scan d s j l
    else begin
      let (i1, i2) = d in
      String.blit s i2 s i1 (i - i2);
      let i1 = i1 + i - i2 in
      let (v'', i) = compose_rec s j l v'' in
      let i1 = encode_char s i1 l v'' in
      scan (i1, i) s i l
    end
  end else
    scan d s j l

and compose_rec s i l v =
  try
    let (v', j) = decode_char s i l in
    if unicode_combinable v' then begin
      let v'' = combine v v' in
      if v'' = 0 then
        (v, i)
      else
        compose_rec s j l v''
    end else
      (v, i)
  with Invalid ->
    (v, i)

let compose s =
  try scan (0, 0) (String.copy s) 0 (String.length s) with Invalid -> s

(***)

let set_2 s i v =
  set s i (v land 0xff);
  set s (i + 1) (v lsr 8)

let get_2 s i = (get s (i + 1)) lsl 8 + get s i

let rec scan s' j s i l =
  if i < l then begin
    let c = get s i in
    if c < 0x80 then
      cont s' j s (i + 1) l c
    else if c < 0xE0 then begin
      (* 80 - 7FF *)
      if c < 0xc2 || i + 1 >= l then fail () else
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then fail () else
      let v = c lsl 6 + c1 - 0x3080 in
      cont s' j s (i + 2) l v
    end else if c < 0xF0 then begin
      (* 800 - FFFF *)
      if i + 2 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      if (c1 lor c2) land 0xc0 <> 0x80 then fail () else
      let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
      if v < 0x800 then fail () else
      cont s' j s (i + 3) l v
    end else begin
      (* 10000 - 10FFFF *)
      if i + 3 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      let c3 = get s (i + 3) in
      if (c1 lor c2 lor c3) land 0xc0 <> 0x80 then fail () else
      let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
      if v < 0x10000 || v > 0x10ffff then fail () else
      let v = v - 0x10000 in
      set_2 s' j (v lsr 10 + 0xD800);
      set_2 s' (j + 2) (v land 0x3FF + 0xDC00);
      scan s' (j + 4) s (i + 4) l
    end
  end else
    String.sub s' 0 (j + 2)

and cont s' j s i l v =
  set_2 s' j v;
  scan s' (j + 2) s i l

let to_utf_16 s =
  let l = String.length s in
  let s' = String.make (2 * l + 2) '\000' in
  scan s' 0 s 0 l

(***)

let sfm_encode =
  [| 0x0000; 0xf001; 0xf002; 0xf003; 0xf004; 0xf005; 0xf006; 0xf007;
     0xf008; 0xf009; 0xf00a; 0xf00b; 0xf00c; 0xf00d; 0xf00e; 0xf00f;
     0xf010; 0xf011; 0xf012; 0xf013; 0xf014; 0xf015; 0xf016; 0xf017;
     0xf018; 0xf019; 0xf01a; 0xf01b; 0xf01c; 0xf01d; 0xf01e; 0xf01f;
     0x0020; 0x0021; 0xf020; 0x0023; 0x0024; 0x0025; 0x0026; 0x0027;
     0x0028; 0x0029; 0xf021; 0x002b; 0x002c; 0x002d; 0x002e; 0x002f;
     0x0030; 0x0031; 0x0032; 0x0033; 0x0034; 0x0035; 0x0036; 0x0037;
     0x0038; 0x0039; 0xf022; 0x003b; 0xf023; 0x003d; 0xf024; 0xf025;
     0x0040; 0x0041; 0x0042; 0x0043; 0x0044; 0x0045; 0x0046; 0x0047;
     0x0048; 0x0049; 0x004a; 0x004b; 0x004c; 0x004d; 0x004e; 0x004f;
     0x0050; 0x0051; 0x0052; 0x0053; 0x0054; 0x0055; 0x0056; 0x0057;
     0x0058; 0x0059; 0x005a; 0x005b; 0xf026; 0x005d; 0x005e; 0x005f;
     0x0060; 0x0061; 0x0062; 0x0063; 0x0064; 0x0065; 0x0066; 0x0067;
     0x0068; 0x0069; 0x006a; 0x006b; 0x006c; 0x006d; 0x006e; 0x006f;
     0x0070; 0x0071; 0x0072; 0x0073; 0x0074; 0x0075; 0x0076; 0x0077;
     0x0078; 0x0079; 0x007a; 0x007b; 0xf027; 0x007d; 0x007e; 0x007f |]

let set_2 s i v =
  set s i (v land 0xff);
  set s (i + 1) (v lsr 8)

let get_2 s i = (get s (i + 1)) lsl 8 + get s i

let end_of_name s i l = let i' = i + 1 in i' = l || get s i' = 0x2f (*'/'*)

let rec scan s' j s i l =
  if i < l then begin
    let c = get s i in
    if c < 0x80 then
      cont s' j s (i + 1) l
        (if c = 0x20 && end_of_name s i l then 0xf028
         else if c = 0x2e && end_of_name s i l then 0xf029
         else Array.unsafe_get sfm_encode c)
    else if c < 0xE0 then begin
      (* 80 - 7FF *)
      if c < 0xc2 || i + 1 >= l then fail () else
      let c1 = get s (i + 1) in
      if c1 land 0xc0 <> 0x80 then fail () else
      let v = c lsl 6 + c1 - 0x3080 in
      cont s' j s (i + 2) l v
    end else if c < 0xF0 then begin
      (* 800 - FFFF *)
      if i + 2 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      if (c1 lor c2) land 0xc0 <> 0x80 then fail () else
      let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
      if v < 0x800 then fail () else
      cont s' j s (i + 3) l v
    end else begin
      (* 10000 - 10FFFF *)
      if i + 3 >= l then fail () else
      let c1 = get s (i + 1) in
      let c2 = get s (i + 2) in
      let c3 = get s (i + 3) in
      if (c1 lor c2 lor c3) land 0xc0 <> 0x80 then fail () else
      let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
      if v < 0x10000 || v > 0x10ffff then fail () else
      let v = v - 0x10000 in
      set_2 s' j (v lsr 10 + 0xD800);
      set_2 s' (j + 2) (v land 0x3FF + 0xDC00);
      scan s' (j + 4) s (i + 4) l
    end
  end else
    String.sub s' 0 (j + 2)

and cont s' j s i l v =
  set_2 s' j v;
  scan s' (j + 2) s i l

let to_utf_16_filename s =
  let l = String.length s in
  let s' = String.make (2 * l + 2) '\000' in
  scan s' 0 s 0 l

(****)

let rec scan s' i' l' s i l =
  if i + 2 <= l then begin
    let v = get_2 s i in
    if v = 0 then
      String.sub s' 0 i'  (* null *)
    else if v < 0xD800 || v > 0xDFFF then
      let i' = encode_char s' i' l' v in
      scan s' i' l' s (i + 2) l
    else if v >= 0xdc00 || i + 4 > l then
      let i' = encode_char s' i' l' v in
      scan s' i' l' s (i + 2) l
(*      fail ()  *)
    else begin
      let v' = get_2 s (i + 2) in
      if v' < 0xDC00 || v' > 0XDFFF then
        let i' = encode_char s' i' l' v in
        scan s' i' l' s (i + 2) l
(*        fail ()*)
      else
        let i' =
          encode_char s' i' l' ((v - 0xD800) lsl 10 + (v' - 0xDC00) + 0x10000)
        in
        scan s' i' l' s (i + 4) l
    end
  end else if i < l then
    fail () (* Odd number of chars *)
  else
    String.sub s' 0 i'

let from_utf_16 s =
  let l = String.length s in
  let l' = 3 * l / 2 in
  let s' = String.create l' in
  scan s' 0 l' s 0 l

(****)

let end_of_name s i l =
  i + 2 = l || (i + 4 <= l && s.[i + 2] = '/' && s.[i + 3] = '\000')

let sfm_decode =
  "\x00\x01\x02\x03\x04\x05\x06\x07\
   \x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\
   \x10\x11\x12\x13\x14\x15\x16\x17\
   \x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\
   \"*:<>?\\| ."

let rec scan s' i' l' s i l =
  if i + 2 <= l then begin
    let v = get_2 s i in
    if v = 0 then
      String.sub s' 0 i'  (* null *)
    else if v < 0xD800 then
      let i' = encode_char s' i' l' v in
      scan s' i' l' s (i + 2) l
    else if v > 0xDFFF then begin
      let v =
        if v > 0xf000 && v <= 0xf029 then
          if v = 0xf028 && end_of_name s i l then 0x20
          else if v = 0xf029 && end_of_name s i l then 0x2e
          else get sfm_decode (v - 0xf000)
        else
          v
      in
      let i' = encode_char s' i' l' v in
      scan s' i' l' s (i + 2) l
    end else if v >= 0xdc00 || i + 4 > l then
      let i' = encode_char s' i' l' v in
      scan s' i' l' s (i + 2) l
(*      fail ()  *)
    else begin
      let v' = get_2 s (i + 2) in
      if v' < 0xDC00 || v' > 0XDFFF then
        let i' = encode_char s' i' l' v in
        scan s' i' l' s (i + 2) l
(*        fail ()*)
      else
        let i' =
          encode_char s' i' l' ((v - 0xD800) lsl 10 + (v' - 0xDC00) + 0x10000)
        in
        scan s' i' l' s (i + 4) l
    end
  end else if i < l then
    fail () (* Odd number of chars *)
  else
    String.sub s' 0 i'

(* NOTE: we MUST have to_utf_16_filename (from_utf_16 s) = s for any
   Windows valid filename s *)
let from_utf_16_filename s =
  let l = String.length s in
  let l' = 3 * l / 2 in
  let s' = String.create l' in
  scan s' 0 l' s 0 l

(****)

let rec scan s i l =
  i = l ||
  let c = get s i in
  if c < 0x80 then
    c <> 0 && scan s (i + 1) l
  else if c < 0xE0 then begin
    (* 80 - 7FF *)
    c >= 0xc2 && i + 1 < l &&
    let c1 = get s (i + 1) in
    c1 land 0xc0 = 0x80 &&
    scan s (i + 2) l
  end else if c < 0xF0 then begin
    (* 800 - FFFF *)
    i + 2 < l &&
    let c1 = get s (i + 1) in
    let c2 = get s (i + 2) in
    (c1 lor c2) land 0xc0 = 0x80 &&
    let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
    v >= 0x800 && (v < 0xd800 || (v > 0xdfff && v <> 0xfffe && v <> 0xffff)) &&
    scan s (i + 3) l
  end else begin
    (* 10000 - 10FFFF *)
    i + 3 < l &&
    let c1 = get s (i + 1) in
    let c2 = get s (i + 2) in
    let c3 = get s (i + 3) in
    (c1 lor c2 lor c3) land 0xc0 = 0x80 &&
    let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
    v >= 0x10000 && v <= 0x10ffff &&
    scan s (i + 4) l
  end

let check_utf_8 s = scan s 0 (String.length s)

(****)

let wf_utf8 =
  [[('\x01', '\x7F')];
   [('\xC2', '\xDF'); ('\x80', '\xBF')];
   [('\xE0', '\xE0'); ('\xA0', '\xBF'); ('\x80', '\xBF')];
   [('\xE1', '\xEC'); ('\x80', '\xBF'); ('\x80', '\xBF')];
   [('\xED', '\xED'); ('\x80', '\x9F'); ('\x80', '\xBF')];
   [('\xEE', '\xEF'); ('\x80', '\xBF'); ('\x80', '\xBF')];
   [('\xF0', '\xF0'); ('\x90', '\xBF'); ('\x80', '\xBF'); ('\x80', '\xBF')];
   [('\xF1', '\xF3'); ('\x80', '\xBF'); ('\x80', '\xBF'); ('\x80', '\xBF')];
   [('\xF4', '\xF4'); ('\x80', '\x8F'); ('\x80', '\xBF'); ('\x80', '\xBF')]]

let rec accept_seq l s i len =
  match l with
    [] ->
      Some i
  | (a, b) :: r ->
      if i = len || s.[i] < a || s.[i] > b then
        None
      else
        accept_seq r s (i + 1) len

let rec accept_rec l s i len =
  match l with
    [] ->
      None
  | seq :: r ->
      match accept_seq seq s i len with
        None -> accept_rec r s i len
      | res  -> res

let accept = accept_rec wf_utf8

(***)

let protect_char buf c =
  if c = '\x00' then
    Buffer.add_char buf ' '
  else if c < '\x80' then
    Buffer.add_char buf c
  else
    let c = Char.code c in
    Buffer.add_char buf (Char.chr (c lsr 6 + 0xC0));
    Buffer.add_char buf (Char.chr (c land 0x3f + 0x80))

let rec protect_rec buf s i len =
  if i = len then
    Buffer.contents buf
  else
    match accept s i len with
      Some i' ->
        Buffer.add_substring buf s i (i' - i);
        protect_rec buf s i' len
    | None ->
        protect_char buf s.[i];
        protect_rec buf s (i + 1) len

let expl f s = f s 0 (String.length s)

(* Convert a string to UTF8 by keeping all UTF8 characters unchanged
   and considering all other characters as ISO 8859-1 characters *)
let protect s =
  let buf = Buffer.create (String.length s * 2) in
  expl (protect_rec buf) s
