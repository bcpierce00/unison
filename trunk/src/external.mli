(* Unison file synchronizer: src/external.mli *)
(* Copyright 1999-2008 (see COPYING for details) *)

val runExternalProgram : string -> Unix.process_status * string
val readChannelTillEof : in_channel -> string
