(* Unison file synchronizer: src/files.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

(* As usual, these functions should only be called by the client (i.e., in   *)
(* the same address space as the user interface).                            *)

(* Delete the given subtree of the given replica                             *)
val delete :
     Common.root                 (* source root *)
  -> Path.t                      (* deleted path *)
  -> Common.root                 (* root *)
  -> Path.t                      (* path to delete *)
  -> Common.updateItem           (* updates that will be discarded *)
  -> unit Lwt.t

(* Region used for the copying. Exported to be correctly set in transport.ml *)
(* to the maximum number of threads                                          *)
val copyReg : Lwt_util.region

(* Copy a path in one replica to another path in a second replica.  The copy *)
(* is performed atomically (or as close to atomically as the os will         *)
(* support) using temporary files.                                           *)
val copy :
     [`Update of (Uutil.Filesize.t * Uutil.Filesize.t) | `Copy]
                                (* whether there was already a file *)
  -> Common.root                (* from what root *)
  -> Path.t                     (* from what path *)
  -> Common.updateItem          (* source updates *)
  -> Common.root                (* to what root *)
  -> Path.t                     (* to what path *)
  -> Common.updateItem          (* dest. updates *)
  -> Uutil.File.t               (* id for showing progress of transfer *)
  -> unit Lwt.t

(* Copy the permission bits from a path in one replica to another path in a  *)
(* second replica.                                                           *)
val setProp :
     Common.root                (* source root *)
  -> Path.t                     (* source path *)
  -> Common.root                (* target root *)
  -> Path.t                     (* target path *)
  -> Props.t                    (* previous properties *)
  -> Props.t                    (* new properties *)
  -> Common.updateItem          (* source updates *)
  -> Common.updateItem          (* target updates *)
  -> unit Lwt.t

(* Generate a difference summary for two (possibly remote) versions of a     *)
(* file and send it to a given function                                      *)
val diff :
     Common.root                (* first root *)
  -> Path.t                     (* path on first root *)
  -> Common.updateItem          (* first root updates *)
  -> Common.root                (* other root *)
  -> Path.t                     (* path on other root *)
  -> Common.updateItem          (* target updates *)
  -> (string->string->unit)     (* how to display the (title and) result *)
  -> Uutil.File.t               (* id for showing progress of transfer *)
  -> unit

(* This should be called at the beginning of execution, to detect and clean  *)
(* up any pending file operations left over from previous (abnormally        *)
(* terminated) synchronizations                                              *)
val processCommitLogs : unit -> unit

(* List the files in a directory matching a pattern.  (It would be better
   to use fspath, etc., here instead of string) *)
val ls : string -> string -> string list

val get_files_in_directory : string -> string list

val merge :
     Common.root                  (* first root *)
  -> Common.root                  (* second root *)
  -> Path.t                       (* path to merge *)
  -> Uutil.File.t                 (* id for showing progress of transfer *)
  -> Common.updateItem            (* differences from the archive *)
  -> Common.updateItem            (* ... *)
  -> (string->string->bool)       (* function to display the (title and) result 
				     and ask user for confirmation (when -batch 
				     is true, the function should not ask any 
				     questions and should always return true) *)
  -> unit
