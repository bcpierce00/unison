
(* Clear the list of aborted items. *)
val reset : unit -> unit

(* Abort transfer for either one particular item or all items. *)
(* These functions should only be called on the client. *)
val file : Uutil.File.t -> unit
val all : unit -> unit

(* Check whether stop of all transfers has been requested. *)
val isAll : unit -> bool
val checkAll : unit -> unit (* Raises a transient exception *)

(* Check whether an item is being aborted.  A transient exception is
   raised if this is the case. *)
val check : Uutil.File.t -> unit

(* Test whether the exception is an abort exception. *)
val testException : exn -> bool
