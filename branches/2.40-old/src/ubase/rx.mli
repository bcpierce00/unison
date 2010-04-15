(* Unison file synchronizer: src/ubase/rx.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

type t

(* Posix regular expression *)
val rx : string -> t

(* File globbing *)
val glob : string -> t
val glob' : bool -> string -> t
   (* Same, but allows to choose whether dots at the beginning of a
      file name need to be explicitly matched (true) or not (false) *)
val globx : string -> t
val globx' : bool -> string -> t
    (* These two functions also recognize the pattern {...} *)

(* String expression (literal match) *)
val str : string -> t

(* Operations on regular expressions *)
val alt : t list -> t                  (* Alternative *)
val seq : t list -> t                  (* Sequence *)
val empty : t                          (* Match nothing *)
val epsilon : t                        (* Empty word *)
val rep : t -> int -> int option -> t  (* Repeated matches *)
val rep0 : t -> t                      (* 0 or more matches *)
val rep1 : t -> t                      (* 1 or more matches *)
val opt : t -> t                       (* 0 or 1 matches *)
val bol : t                            (* Beginning of line *)
val eol : t                            (* End of line *)
val any : t                            (* Any character *)
val notnl : t                          (* Any character but a newline *)
val set : string -> t                  (* Any character of the string *)
val inter : t list -> t                (* All subexpressions must match *)
val diff : t -> t -> t                 (* The first expression matches
                                          but not the second *)
val case_insensitive : t -> t          (* Case insensitive matching *)

(* Test whether a regular expression matches a string  *)
val match_string : t -> string -> bool

(* Test whether a regular expression matches a substring of the given
   string *)
val match_substring : t -> string -> bool

(* Test whether a regular expression matches some characters of a
   string starting at a given position.  Return the length of
   the matched prefix. *)
val match_prefix : t -> string -> int -> int option

(* Errors that can be raised during the parsing of Posix regular
   expressions *)
exception Parse_error
exception Not_supported
