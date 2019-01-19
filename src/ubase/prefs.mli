(* Unison file synchronizer: src/ubase/prefs.mli *)
(* $I3: Copyright 1999-2002 (see COPYING for details) $ *)

type 'a t

val read : 'a t -> 'a
val set : 'a t -> 'a -> unit
val name : 'a t -> string list
val overrideDefault : 'a t -> 'a -> unit
val readDefault : 'a t -> 'a

(* Convenient functions for registering simple kinds of preferences.  Note   *)
(* that createStringPref creates a preference that can only be set once,     *)
(* while createStringListPref creates a reference to a list of strings that  *)
(* accumulates a list of values.                                             *)
val createBool :
        string              (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> bool                (* initial value *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> bool t              (*   -> new preference value *)

val createInt :
        string              (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> int                 (* initial value *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> int t               (*   -> new preference value *)

val createString :
        string              (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> string              (* initial value *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> string t            (*   -> new preference value *)

val createFspath :
        string              (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> System.fspath       (* initial value *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> System.fspath t     (*   -> new preference value *)

val createStringList :
        string              (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> string list t       (*   -> new preference value *)

val createBoolWithDefault :
        string              (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> [`True|`False|`Default] t
                            (*   -> new preference value *)

exception IllegalValue of string
(* A more general creation function that allows arbitrary functions for      *)
(* interning and printing values.  The interning function should raise       *)
(* IllegalValue if it is passed a string it cannot deal with.                *)
val create :
        string                  (* preference name *)
     -> ?local:bool             (* whether it is local to the client *)
     -> 'a                      (* initial value *)
     -> string                  (* documentation string *)
     -> string                  (* full (tex) documentation string *)
     -> ('a->string->'a)        (* interning function for preference values
                                   (1st arg is old value of preference) *)
     -> ('a -> string list)     (* printing function for preference values *)
     -> 'a t                    (*   -> new preference value *)

(* Create an alternate name for a preference (the new name will not appear   *)
(* in usage messages or generated documentation)                             *)
val alias : 'a t              (* existing preference *)
         -> string            (* new name *)
         -> unit

(* Reset all preferences to their initial values                             *)
val resetToDefaults : unit -> unit

(* ------------------------------------------------------------------------- *)

(* Parse command-line arguments, exiting program if there are any problems.  *)
(* If a StringList preference named "rest" has been registered, then any     *)
(* anonymous arguments on the command line will be added to its value.       *)
val parseCmdLine :
     string             (* Usage message *)
  -> unit

(* Make a preliminary scan without setting any preferences                   *)
(* Note: Command line include options are not processed; they will appear in *)
(* the map.                                                                  *)
val scanCmdLine : string -> (string list) Util.StringMap.t

val printUsage : string -> unit

(* ---------------------------------------------------------------------- *)

(* The name of the preferences file (if any), not including the .prf         *)
val profileName : string option ref

(* Calculate the full pathname of a preference file                          *)
val profilePathname : ?add_ext:bool -> string -> System.fspath

(* Check whether the profile file is unchanged                               *)
val profileUnchanged : unit -> bool

(* Add a new preference to the file on disk (the result is a diagnostic      *)
(* message that can be displayed to the user to verify where the new pref    *)
(* went)                                                                     *)
val add : string -> string -> string

(* Add a comment line to the preferences file on disk                        *)
val addComment : string -> unit

(* Scan a given preferences file and return a list of tuples of the form     *)
(* (fileName, lineno, name, value), without changing any of the preferences  *)
val readAFile : ?fail:bool -> ?add_ext:bool -> string
     -> (string * int * string * string) list

(* Parse the preferences file, raising Fatal if there are any problems       *)
val loadTheFile : unit -> unit

(* Parse the given strings as if they were part of the preferences file      *)
val loadStrings : string list -> unit

(* ------------------------------------------------------------------------- *)

type dumpedPrefs

(* Dump current values of all preferences into a value that can be
   marshalled and sent over the network or stored in a file for fast
   retrieval *)
val dump : unit -> dumpedPrefs

(* Load new values of all preferences from a string created by dump          *)
val load : dumpedPrefs -> unit

(* ------------------------------------------------------------------------- *)

type typ =
  [`BOOL | `INT | `STRING | `STRING_LIST | `BOOLDEF | `CUSTOM | `UNKNOWN]

val canonicalName : string -> string
val typ : string -> typ
val documentation : string -> string * string * bool
val list : unit -> string list

(* ------------------------------------------------------------------------- *)

val printFullDocs : unit -> unit
val dumpPrefsToStderr : unit -> unit
