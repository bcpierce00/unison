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
val repeat : string Prefs.t

(* User preference: Try failing paths N times *)
val retry : int Prefs.t

(* User preference: confirmation before committing merge results *)
val confirmmerge : bool Prefs.t

val runTestsPrefName : string

(* Format the information about current contents of a path in one replica (the second argument
   is used as a separator) *)
val details2string : Common.reconItem -> string -> string

(* Format a path, eliding initial components that are the same as the
   previous path *)
val displayPath : Path.t -> Path.t -> string

(* Format the names of the roots for display at the head of the
   corresponding columns in the UI *)
val roots2string : unit -> string

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

val uiInit :
    ?prepDebug:(unit -> unit) ->
    reportError:(string -> unit) ->
    displayWaitMessage:(unit -> unit) ->
    getProfile:(unit -> string option) ->
    getFirstRoot:(unit -> string option) ->
    getSecondRoot:(unit -> string option) ->
    termInteract:(string -> string -> string) option ->
    unit ->
    unit

val uiInitStage1 :
    ?prepDebug:(unit -> unit) ->
    reportError:(string -> unit) ->
    getProfile:(unit -> string option) ->
    unit ->
    string

val uiInitStage2 :
    ?prepDebug:(unit -> unit) ->
    profileName:string ->
    displayWaitMessage:(unit -> unit) ->
    getFirstRoot:(unit -> string option) ->
    getSecondRoot:(unit -> string option) ->
    termInteract:(string -> string -> string) option ->
    unit ->
    unit

val initPrefs :
  profileName:string ->
  displayWaitMessage:(unit->unit) ->
  getFirstRoot:(unit->string option) ->
  getSecondRoot:(unit->string option) ->
  ?prepDebug:(unit -> unit) ->
  termInteract:(string -> string -> string) option ->
  unit ->
  unit

(* Make sure remote connections (if any) corresponding to active roots
   are still established and re-establish them if necessary.
   [refreshConnection] is like [initPrefs] but without reloading the profile
   and re-initializing the prefs. *)
val refreshConnection :
  displayWaitMessage:(unit -> unit) ->
  termInteract:(string -> string -> string) option ->
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
