(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
val create_session :
    string -> string array
      -> Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
      (Unix.file_descr option * int)

(* Watch a terminal (as returned by create_session) and a regular
   input until there is regular input but no terminal input.  Pass any
   terminal input to the callback and make sure the terminal is
   closed. *)
val termInteract :
    Unix.file_descr -> Unix.file_descr -> (string -> string) -> unit

(* termInput fdTerm fdInput
   Wait until there is input on at least one file descriptor.
   If there is terminal input s, return Some s.
   Otherwise, return None. *)
val termInput :
    Unix.file_descr -> Unix.file_descr -> string option

(* For recognizing messages from OpenSSH *)
val password : string -> bool
val authenticity : string -> bool
