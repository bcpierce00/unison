(* Unison file synchronizer: src/uicommon.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

(* Kinds of UI *)
type interface =
   Text
 | Graphic

val minterface : interface Umarshal.t

(* The interface of a concrete UI implementation *)
module type UI =
sig
  val start : interface -> unit
  val defaultUi : interface
end

(* User preference: when true, ask fewer questions *)
val auto : bool Prefs.t

(* User preference: How tall to make the main window in the GTK ui *)
val mainWindowHeight : int Prefs.t

(* User preference: Expert mode *)
val expert : bool Prefs.t

(* User preference: Whether to display 'contacting server' message *)
val contactquietly : bool Prefs.t

(* User preference: The 'contacting server' message itself *)
val contactingServerMsg : unit -> string

(* User preference: Descriptive label for this profile *)
val profileLabel : string Prefs.t

(* User preference: Synchronize repeatedly *)
val repeat : [ `NoRepeat | `Interval of int | `Watch
  | `WatchAndInterval of int | `Invalid of string * exn ] Prefs.t

(* User preference: Try failing paths N times *)
val retry : int Prefs.t

(* User preference: confirmation before committing merge results *)
val confirmmerge : bool Prefs.t

val runTestsPrefName : string

val runtests : bool Prefs.t

val testServer : bool Prefs.t

(* Format the information about current contents of a path in one replica (the second argument
   is used as a separator) *)
val details2string : Common.reconItem -> string -> string

(* Format a path, eliding initial components that are the same as the
   previous path *)
val displayPath : Path.t -> Path.t -> string

(* Format the names of the roots for display at the head of the
   corresponding columns in the UI *)
val roots2string : unit -> string
val roots2niceStrings : int -> Common.root * Common.root -> string * string

(* Format a reconItem (and its status string) for display, eliding
   initial components that are the same as the previous path *)
val reconItem2string : Path.t -> Common.reconItem -> string -> string

type action = AError | ASkip of bool | ALtoR of bool | ARtoL of bool | AMerge

(* Same as previous function, but returns a tuple of strings *)
val reconItem2stringList :
  Path.t -> Common.reconItem -> string * action * string * string

(* Format an exception for display *)
val exn2string : exn -> string

(* Calculate and display differences for a file *)
val showDiffs :
     Common.reconItem           (* what path *)
  -> (string->string->unit)     (* how to display the (title and) result *)
  -> (string->unit)             (* how to display errors *)
  -> Uutil.File.t               (* id for transfer progress reports *)
  -> unit

val dangerousPathMsg : Path.t list -> string

(* Utilities for adding ignore patterns *)
val ignorePath : Path.t -> string
val ignoreName : Path.t -> string
val ignoreExt  : Path.t -> string
val addIgnorePattern : string -> unit

val usageMsg : string

val shortUsageMsg : string

val uiInitClRootsAndProfile :
    ?prepDebug:(unit -> unit) ->
    unit ->
    (string option, string) result

val initPrefs :
  profileName:string ->
  promptForRoots:(unit -> (string * string) option) ->
  ?prepDebug:(unit -> unit) ->
  unit ->
  unit

val clearClRoots : unit -> unit

(* Make sure remote connections (if any) corresponding to active roots
   are established and (re-)establish them if necessary.
   [initPrefs] must be called before [connectRoots]. *)
val connectRoots :
  ?termInteract:(string -> Terminal.termInteract) ->
  displayWaitMessage:(unit -> unit) ->
  unit ->
  unit

val validateAndFixupPrefs : unit -> unit Lwt.t

(* Exit codes *)
val perfectExit: int   (* when everything's okay *)
val skippyExit: int    (* when some items were skipped, but no failure occurred *)
val failedExit: int    (* when there's some non-fatal failure *)
val fatalExit: int     (* when fatal failure occurred *)
val exitCode: bool * bool -> int
(* (anySkipped?, anyFailure?) -> exit code *)

(* Initialization *)
val testFunction : (unit->unit) ref

(* Profile scanning and selection *)
type profileInfo = {roots:string list; label:string option; key:string option}

val profileKeymap : (string * profileInfo) option array

val profilesAndRoots : (string * profileInfo) list ref

val scanProfiles : unit -> unit

(* Update propagation *)
val transportStart : unit -> unit
val transportFinish : unit -> unit

val transportItems : 'a array -> ('a -> bool) -> (int -> 'a -> unit Lwt.t) -> unit

(* Statistics of update propagation *)
module Stats : sig
  type t
  val init :
       Uutil.Filesize.t (* Total size to complete 100% *)
    -> t
  val update :
       t
    -> float            (* Current absolute time *)
    -> Uutil.Filesize.t (* Current completed size (not delta) *)
    -> unit
  val curRate : t -> float (* Current progress rate, very volatile *)
  val avgRate1 : t -> float (* Moving average of the rate above, more stable *)
  val avgRate2 : t -> float (* Double moving average, very stable *)
  val eta : t -> ?rate:float -> string -> string (* Defaults to rate2 *)
end
