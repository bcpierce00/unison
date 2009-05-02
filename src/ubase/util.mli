(* Unison file synchronizer: src/ubase/util.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* Miscellaneous utility functions and datatypes *)

(* ---------------------------------------------------------------------- *)
(* Exceptions *)

exception Fatal of string
exception Transient of string

val encodeException : string -> [`Transient | `Fatal] -> exn -> 'a
val convertUnixErrorsToTransient : string -> (unit -> 'a) -> 'a
val convertUnixErrorsToFatal : string -> (unit -> 'a) -> 'a
val ignoreTransientErrors : (unit -> unit) -> unit

(* [unwindProtect e1 e2] executes e1, catching the above two exceptions and
   executing e2 (passing it the exception packet, so that it can log a
   message or whatever) before re-raising them *)
val unwindProtect : (unit -> 'a) -> (exn -> unit) -> 'a

(* [finalize e1 e2] executes e1 and then e2.  If e1 raises either of the
   above two exceptions e2 is still executed and the exception is reraised *)
val finalize : (unit -> 'a) -> (unit -> unit) -> 'a

(* For data structures that need to record when operations have succeeded or
   failed *)
type confirmation =
   Succeeded
 | Failed of string

val printException : exn -> string

val process_status_to_string : Unix.process_status -> string

(* ---------------------------------------------------------------------- *)
(* Strings *)

(* Case insensitive comparison *)
val nocase_cmp : string -> string -> int
val nocase_eq  : string -> string -> bool

(* Ready-build set and map implementations *)
module StringSet : Set.S with type elt = string
module StringMap : Map.S with type key = string
val stringSetFromList : string list -> StringSet.t

(* String manipulation *)
val truncateString : string -> int -> string
val startswith : string -> string -> bool
val endswith : string -> string -> bool
val findsubstring : string -> string -> int option
val replacesubstring : string -> string -> string -> string (* IN,FROM,TO *)
val replacesubstrings : string -> (string * string) list -> string
val concatmap : string -> ('a -> string) -> 'a list -> string
val trimWhitespace : string -> string
val splitIntoWords : string -> char -> string list
val splitIntoWordsByString : string -> string -> string list
val padto : int -> string -> string

(* ---------------------------------------------------------------------- *)
(* Miscellaneous *)

(* Architecture *)
val osType : [`Unix | `Win32]
val isCygwin: bool    (* osType will be `Win32 in this case *)

(* Options *)
val extractValueFromOption : 'a option -> 'a
val option2string: ('a -> string) -> ('a option -> string)

(* Miscellaneous *)
val time2string : float -> string
val percentageOfTotal :
  int ->     (* current value *)
  int ->     (* total value *)
  int        (* percentage of total *)
val monthname : int -> string
val percent2string : float -> string
val fileInHomeDir : string -> string

(* Just like the versions in the Unix module, but raising Transient
   instead of Unix_error *)
val localtime : float -> Unix.tm
val time : unit -> float

(* Global debugging printer (it's exposed as a ref so that modules loaded
   before Trace can use it; the ref will always be set to Some(Trace.debug)) *)
val debugPrinter : ((string -> (unit->unit) -> unit) option) ref
(* A synonym for Trace.debug *)
val debug : string -> (unit->unit) -> unit

(* The UI must supply a function to warn the user *)
val warnPrinter : (string -> unit) option ref
val warn : string -> unit

(* Someone should supply a function here that will convert a simple filename
   to a filename in the unison directory *)
val supplyFileInUnisonDirFn : (string -> string) -> unit
(* Use it like this: *)
val fileInUnisonDir : string -> string

(* Printing and formatting functions *)

val format : ('a, Format.formatter, unit) format -> 'a
(** Format some text on the current formatting channel.
    This is the only formatting function that should be called anywhere in the program! *)

val flush : unit -> unit

val format_to_string : (unit -> unit) -> string 
(** [format_to_string f] runs [f] in a context where the Format functions are redirected to
    a string, which it returns. *)

(* Format and print messages on the standard error stream, being careful to
   flush the stream after each one *)
val msg : ('a, out_channel, unit) format -> 'a

(* Set the info line *)
val set_infos : string -> unit
