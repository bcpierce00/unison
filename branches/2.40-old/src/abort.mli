
(* Clear the list of aborted item. *)
val reset : unit -> unit

(* Abort transfer for either one particular item or all items. *)
(* These functions should only be called on the client. *)
val file : Uutil.File.t -> unit
val all : unit -> unit

(* Check whether an item is being aborted.  A transient exception is
   raised if this is the case. *)
val check : Uutil.File.t -> unit

(* Test whether the exeption is an abort exception. *)
val testException : exn -> bool
