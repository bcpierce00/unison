(* Unison file synchronizer: src/external.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

val runExternalProgram : string -> (Unix.process_status * string) Lwt.t
val runExternalProgramArgs : string -> string array -> (Unix.process_status * string) Lwt.t
val readChannelTillEof : in_channel -> string
