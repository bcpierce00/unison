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

(* Convert to and from little-endian UTF-16 encoding *)
(*XXX What about null-termination? *)
val to_utf_16 : string -> string
val from_utf_16 : string -> string

(* Check wether the string contains only well-formed UTF-8 characters *)
val check_utf_8 : string -> bool
