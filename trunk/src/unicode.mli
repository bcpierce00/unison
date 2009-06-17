(* Unison file synchronizer: src/unicode.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)


(* Case-insensitive comparison.  If two strings are equal according to
   Mac OS X (Darwin, actually, but the algorithm has hopefully
   remained unchanged) or Windows (Samba), then this function returns 0 *)
val compare : string -> string -> int

(* Corresponding normalization *)
val normalize : string -> string

(* Compose Unicode strings.  This reverts the decomposition performed
   by Mac OS X. *)
val compose : string -> string

(* Convert to and from a null-terminated little-endian UTF-16 string *)
(* Do not fail on isolated surrogate but rather generate ill-formed
   UTF-8 characters, so that the conversion never fails. *)
val to_utf_16 : string -> string
val from_utf_16 : string -> string

(* Convert to and from a null-terminated little-endian UTF-16 string *)
(* Invalid NTFS characters are mapped to characters in the unicode
   private use area *)
(* FIX: not correct at the moment: should deal properly with paths such as
   //?/foo/   c:\foo\bar   ... *)
val to_utf_16_filename : string -> string
val from_utf_16_filename : string -> string

(* Check wether the string contains only well-formed UTF-8 characters *)
val check_utf_8 : string -> bool

(* Convert a string to UTF-8 by keeping all UTF-8 characters unchanged
   and considering all other characters as ISO 8859-1 characters *)
val protect : string -> string
