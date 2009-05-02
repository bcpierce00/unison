(* Unison file synchronizer: src/uicommon.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* Kinds of UI *)
type interface =
   Text
 | Graphic

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

(* User preference: Should we reuse top-level windows as much as possible? *)
val reuseToplevelWindows : bool Prefs.t

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

(* User preference: confirmation before commiting merge results *)
val confirmmerge : bool Prefs.t

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
    reportError:(string -> unit) ->
    tryAgainOrQuit:(string -> bool) ->
    displayWaitMessage:(unit -> unit) ->
    getProfile:(unit -> string option) ->
    getFirstRoot:(unit -> string option) ->
    getSecondRoot:(unit -> string option) ->
    termInteract:(string -> string -> string) option ->
    unit

val initPrefs :
  profileName:string ->
  displayWaitMessage:(unit->unit) ->
  getFirstRoot:(unit->string option) ->
  getSecondRoot:(unit->string option) ->
  termInteract:(string -> string -> string) option ->
  unit

val checkCaseSensitivity : unit -> unit Lwt.t

(* Exit codes *)
val perfectExit: int   (* when everything's okay *)
val skippyExit: int    (* when some items were skipped, but no failure occurred *)
val failedExit: int    (* when there's some non-fatal failure *)
val fatalExit: int     (* when fatal failure occurred *)
val exitCode: bool * bool -> int
(* (anySkipped?, anyFailure?) -> exit code *)

(* Initialization *)
val testFunction : (unit->unit) ref
