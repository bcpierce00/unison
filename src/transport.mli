(* Unison file synchronizer: src/transport.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

(* Size of the pool of threads for executing transport actions. *)
val maxThreads : unit -> int

(* Run tasks concurrently in a pool of threads, aquiring tasks with
   the supplied task dispenser function. The tasks received from
   the task dispenser must not raise uncaught exceptions or return
   with [Lwt.fail]. *)
val run :
     (unit -> (unit -> unit Lwt.t) option) (* Task dispenser *)
  -> unit

(* Executes the actions implied by the reconItem list. *)
val transportItem :
     Common.reconItem                 (* Updates that need to be performed *)
  -> Uutil.File.t                     (* id for progress reports *)
  -> (string->string->bool)           (* fn to display title / result of merge and confirm *)
  -> unit Lwt.t

(* Should be called respectively when starting the synchronization and
   once it is finished *)
val logStart : unit -> unit
val logFinish : unit -> unit
