(* Unison file synchronizer: src/ubase/trace.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* ---------------------------------------------------------------------- *)
(* Debugging support *)

(* Show a low-level debugging message.  The first argument is the
   name of the module from which the debugging message originates: this is
   used to control which messages are printing (by looking at the value of
   the 'debug' preference, a list of strings).  The second argument is a
   thunk that, if executed, should print the actual message to stderr.  Note
   that, since control of debugging depends on preferences, it is not possible
   to see debugging output generated before the preferences have been
   loaded. *)
val debug : string -> (unit->unit) -> unit

val debugmods : string list Prefs.t

(* Check whether a particular debugging flag is enabled *)
val enabled : string -> bool

(* Enable/disable a particular flag *)
val enable : string -> bool -> unit

(* When running in server mode, we use this ref to know to indicate this in
   debugging messages *)
val runningasserver : bool ref

(* Tell the Trace module which local stream to use for tracing and
   debugging messages *)
val redirect : [`Stdout | `Stderr | `FormatStdout] -> unit 

(* ---------------------------------------------------------------------- *)
(* Tracing *)

(* The function used to display a message on the machine where the
   user is going to see it.  The default value just prints the string
   on stderr.  The graphical user interface should install an
   appropriate function here when it starts.  In the server process, this
   variable's value is ignored. *) 
val messageDisplayer : (string -> unit) ref

(* The function used to format a status message (with a major and a minor
   part) into a string for display.  Should be set by the user interface. *)
val statusFormatter : (string -> string -> string) ref

(* The internal type of messages (it is exposed because it appears in the
   types of the following) *)
type msg

(* The internal routine used for formatting a message to be displayed
   locally.  It calls !messageDisplayer to do the actual work. *)
val displayMessageLocally : msg -> unit

(* This can be set to function that should be used to get messages to
   the machine where the user can see it, if we are running on some
   other machine.  (On the client machine, this variable's value is None.
   On the server, it should be set to something that moves the message
   across the network and then calls displayMessageLocally on the
   client.) *)
val messageForwarder : (msg -> unit) option ref

(* Allow outside access to the logging preference, so that the main program
   can turn it off by default *)
val logging : bool Prefs.t

(* ---------------------------------------------------------------------- *)
(* Messages *)

(* Suppress all message printing *)
val terse
  : bool Prefs.t

(* Show a string to the user. *)
val message : string -> unit

(* Show a change of "top-level" status (what phase we're in) *)
val status : string -> unit

(* Show a change of "detail" status (what file we're working on) *)
val statusMinor : string -> unit

(* Show a change of "detail" status unless we want to avoid generating
   too much output (e.g. because we're using the text ui) *)
val statusDetail : string -> unit

(* Write a message just to the log file (no extra '\n' will be added: include
   one explicitly if you want one) *)
val log : string -> unit

(* Like 'log', but only send message to log file if -terse preference is set *)
val logverbose : string -> unit

(* When set to true (default), log messages will also be printed to stderr *)
val sendLogMsgsToStderr : bool ref

(* ---------------------------------------------------------------------- *)
(* Timers (for performance measurements during development) *)

type timer

(* Create a new timer, print a description, and start it ticking *)
val startTimer : string -> timer

(* Create a new timer without printing a description *)
val startTimerQuietly : string -> timer

(* Display the current time on a timer (and its description) *)
val showTimer : timer -> unit
