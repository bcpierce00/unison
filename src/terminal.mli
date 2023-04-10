(* Like Unix.create_process except that we also try to set up a
   controlling terminal for the new process.  If successful, a file
   descriptor for the master end of the controlling terminal is
   returned. *)
val create_session :
  string -> string array ->
  Unix.file_descr -> Unix.file_descr -> Unix.file_descr ->
  (Lwt_unix.file_descr * Lwt_unix.file_descr) option * int

val close_session : int -> unit

(* [safe_waitpid] is intended for waiting on child processes that are
   expected to terminate by themselves. If the child process has not
   terminated after a short while then a SIGTERM is sent and if the
   child process still doesn't terminate then a SIGKILL is sent. *)
val safe_waitpid : int -> Unix.process_status

(* termInput fdTerm fdInput
   Wait until there is input on at least one file descriptor.
   If there is terminal input s, return Some s.
   Otherwise, return None. *)
val termInput :
  (Lwt_unix.file_descr * Lwt_unix.file_descr) -> Lwt_unix.file_descr -> string option

type termInteract = {
  userInput : string -> (string -> unit) -> unit;
  endInput : unit -> unit }

val handlePasswordRequests :
  (Lwt_unix.file_descr * Lwt_unix.file_descr) -> termInteract ->
  (unit -> bool) -> unit Lwt.t * (bool -> string Lwt.t)

(* For recognizing messages from OpenSSH *)
val password : string -> bool
val passphrase : string -> bool
val authenticity : string -> bool
