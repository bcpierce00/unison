(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
val create_session :
  string -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  Unix.file_descr option * int

(* termInput fdTerm fdInput
   Wait until there is input on at least one file descriptor.
   If there is terminal input s, return Some s.
   Otherwise, return None. *)
val termInput :
  Unix.file_descr -> Unix.file_descr -> string option

val handlePasswordRequests :
  Unix.file_descr -> (string -> string) -> unit

(* For recognizing messages from OpenSSH *)
val password : string -> bool
val passphrase : string -> bool
val authenticity : string -> bool
