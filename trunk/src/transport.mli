(* Unison file synchronizer: src/transport.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

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
