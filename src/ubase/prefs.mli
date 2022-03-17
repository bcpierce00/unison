(* Unison file synchronizer: src/ubase/prefs.mli *)
(* $I3: Copyright 1999-2002 (see COPYING for details) $ *)

type 'a t

val read : 'a t -> 'a
val set : 'a t -> 'a -> unit
val name : 'a t -> string list
val overrideDefault : 'a t -> 'a -> unit
val readDefault : 'a t -> 'a

type topic = [
  | `General
  | `Sync
  | `Syncprocess
  | `Syncprocess_CLI
  | `CLI
  | `GUI
  | `Remote
  | `Archive ]

type group = [
  | `Basic of topic
  | `Advanced of topic
  | `Expert
  | `Internal of         (* Preferences that are not listed *)
      [ `Pseudo          (* Pseudo-preferences for internal propagation *)
      | `Devel           (* Developer-only or build-related preferences *)
      | `Other ]         (* Other non-listed preferences *)
  ]

(* Note about command line-only preferences. These preferences are never     *)
(* sent to a server (ignoring [local] and [send] arguments). Should a client *)
(* send such a preference anyway then the server silently ignores it.        *)

(* Convenient functions for registering simple kinds of preferences.  Note   *)
(* that createStringPref creates a preference that can only be set once,     *)
(* while createStringListPref creates a reference to a list of strings that  *)
(* accumulates a list of values.                                             *)
val createBool :
        string              (* preference name *)
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)  (* whether preference should be sent to server *)
     -> bool                (* initial value *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> bool t              (*   -> new preference value *)

val createInt :
        string              (* preference name *)
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)  (* whether preference should be sent to server *)
     -> int                 (* initial value *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> int t               (*   -> new preference value *)

val createString :
        string              (* preference name *)
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)  (* whether preference should be sent to server *)
     -> string              (* initial value *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> string t            (*   -> new preference value *)

val createFspath :
        string              (* preference name *)
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)  (* whether preference should be sent to server *)
     -> System.fspath       (* initial value *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> System.fspath t     (*   -> new preference value *)

val createStringList :
        string              (* preference name *)
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)  (* whether preference should be sent to server *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
     -> string              (* documentation string *)
     -> string              (* full (tex) documentation string *)
     -> string list t       (*   -> new preference value *)

val createBoolWithDefault :
        string              (* preference name *)
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)  (* whether preference should be sent to server *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
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
     -> category:group
     -> ?cli_only:bool      (* only a command line option, not in a profile *)
     -> ?local:bool             (* whether it is local to the client *)
     -> ?send:(unit->bool)      (* whether the pref should be sent to server *)
     -> 'a                      (* initial value *)
     -> ?deprecated:bool    (* preference is deprecated (default false) *)
     -> string                  (* documentation string *)
     -> string                  (* full (tex) documentation string *)
     -> ('a->string->'a)        (* interning function for preference values
                                   (1st arg is old value of preference) *)
     -> ('a -> string list)     (* printing function for preference values *)
     -> 'a Umarshal.t
     -> 'a t                    (*   -> new preference value *)

(* Create an alternate name for a preference (the new name will not appear   *)
(* in usage messages or generated documentation)                             *)
val alias : 'a t              (* existing preference *)
         -> string            (* new name *)
         -> unit

(* Mark a preference name as intentionally removed. A removed preference     *)
(* does not exist (can't be specified on command line or in a profile) but   *)
(* will be silently ignored when sent by a remote host (to not break         *)
(* compatibility with old clients).                                          *)
val markRemoved : string -> unit

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
val printUsageForMan : unit -> unit

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
(* ((locName, lineno), name, value), without changing any of the preferences *)
val readAFile : ?fail:bool -> ?add_ext:bool -> string
     -> ((string * int) * string * string) list

(* Parse the preferences file, raising Fatal if there are any problems       *)
val loadTheFile : unit -> unit

(* Parse the given strings as if they were part of the preferences file      *)
val loadStrings : string list -> unit

(* ------------------------------------------------------------------------- *)

type dumpedPrefs

val mdumpedPrefs : dumpedPrefs Umarshal.t

(* Dump current values of all preferences into a value that can be
   marshalled and sent over the network or stored in a file for fast
   retrieval *)
val dump : int -> dumpedPrefs

(* Load new values of all preferences from a string created by dump          *)
val load : dumpedPrefs -> int -> unit

(* ------------------------------------------------------------------------- *)

type typ =
  [`BOOL | `INT | `STRING | `STRING_LIST | `BOOLDEF | `CUSTOM | `UNKNOWN]

val canonicalName : string -> string
val typ : string -> typ
val documentation : string -> string * string
val category : string -> group option
val list : bool -> string list
val topic_title : group -> string

(* ------------------------------------------------------------------------- *)

val printFullDocs : [`TeX | `man] -> unit
val dumpPrefsToStderr : unit -> unit
