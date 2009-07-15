(* Unison file synchronizer: src/case.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

val unicodeEncoding : bool Prefs.t
val useUnicodeAPI : unit -> bool

type mode

val ops : unit ->
  < mode : mode; modeDesc : string;       (* Current mode *)
    compare : string -> string -> int;    (* Comparison function *)
    hash : string -> int;                 (* Hash function compatible with
                                             the comparison function *)
    normalizePattern : string -> string;  (* Normalize a pattern *)
    caseInsensitiveMatch : bool;          (* Whether pattern matching
                                             should be done in a case
                                             insensitive way *)
    normalizeMatchedString : string -> string;
                                          (* Put the string in some form
                                             suitable for pattern matching *)
    normalizeFilename : string -> string; (* Convert a filename into
                                             its preferred form
                                             (NFC for Unicode). *)
    badEncoding : string -> bool >        (* Test whether the string uses
                                             the correct encoding *)

val init : bool -> unit
