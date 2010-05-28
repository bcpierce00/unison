(* Unison file synchronizer: src/recon.mli *)
(* Copyright 1999-2010, Benjamin C. Pierce (see COPYING for details) *)

val reconcileAll :
     ?allowPartial:bool         (* whether we allow partial synchronization
                                   of directories (default to false) *)
  -> ((Path.local * Common.updateItem * Props.t list) *
      (Path.local * Common.updateItem * Props.t list)) list
                                (* one updateItem per replica, per path *)
  -> Common.reconItem list      (* List of updates that need propagated *)
     * bool                     (* Any file updated equally on all roots*)
     * Path.t list              (* Paths which have been emptied on one side*)

(* Use the current values of the '-prefer <ROOT>' and '-force <ROOT>'        *)
(* preferences to override the reconciler's choices                          *)
val overrideReconcilerChoices : Common.reconItem list -> unit

(* If the given reconItem's default direction is Conflict (or the third      *)
(* argument is `Force), then set it as specified by the second argument.     *)
val setDirection :
  Common.reconItem ->
  [`Older | `Newer | `Merge | `Replica1ToReplica2 | `Replica2ToReplica1] ->
  [`Force | `Prefer] ->
  unit

(* Set the given reconItem's direction back to the default                   *)
val revertToDefaultDirection : Common.reconItem -> unit

(* Look up the preferred root and verify that it is OK (this is called at    *)
(* the beginning of the run, before we do anything time consuming, so that   *)
(* we don't have to wait to hear about errors                                *)
val checkThatPreferredRootIsValid : unit -> unit
