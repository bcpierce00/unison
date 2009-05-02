(* Unison file synchronizer: src/pred.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* Predicates over paths.
   
   General description:                                                  

   A predicate is determined by a list of default patterns and a list of 
   current patterns.  These patterns can be modified by                  
   [addDefaultPatterns] and [intern].  Function [test p s] tests whether 
   string [s] satisfies predicate [p], i.e., it matches a pattern of [p].
                                                                         
   For efficiency, the list of patterns are compiled into a regular      
   expression.  Function [test] compares the current value of default    
   patterns and current patterns against the save ones (recorded in      
   last_pref/last_def) to determine whether recompilation is necessary.  
   
   Each pattern has the form
     <TYPE> <PAT> [ -> <ASSOCIATED STRING> ]
   The associated string is ignored by [test] but can be looked up by [assoc].

   Three forms of <TYPE>/<PAT> are recognized:                
   "Name <name>": ..../<name> (using globx)                   
   "Path <path>": <path>, not starting with "/" (using globx) 
   "Regex <regex>": <regex> (using rx)                      
*)


type t

(* Create a new predicate and register it with the preference module.  The first
   arg is the name of the predicate; the second is full (latex) documentation. *)
val create : string -> ?advanced:bool -> string -> t  

(* Check whether a given path matches one of the default or current patterns *)
val test : t -> string -> bool

(* Return the associated string for the first matching pattern.  Raise Not_found
   if no pattern with an associated string matches. *)
val assoc : t -> string -> string

(* Add list of default patterns to the existing list.  (These patterns are
   remembered even when the associated preference is cleared). *)
val addDefaultPatterns : t -> string list -> unit

(* Install a new list of patterns, overriding the current list *)
val intern : t -> string list -> unit

(* Return the current list of patterns *)
val extern : t -> string list

(* Return the current list of associated strings *)
val extern_associated_strings : t -> string list

(* Create an alternate name for a predicate (the new name will not appear 
   in usage messages or generated documentation) *)
val alias : t                 (* existing predicate *)
         -> string            (* new name *)
         -> unit
  
