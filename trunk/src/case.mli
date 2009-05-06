(* Unison file synchronizer: src/case.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

type mode

val ops : unit ->
  < mode : mode;
    compare : string -> string -> int;
    hash : string -> int;
    normalizePattern : string -> string;
    caseInsensitiveMatch : bool;
    normalizeMatchedString : string -> string;
    badEncoding : string -> bool >

val init : bool -> unit
