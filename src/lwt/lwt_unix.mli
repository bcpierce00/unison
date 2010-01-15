(* Module [Lwt_unix]: thread-compatible system calls *)

val sleep : float -> unit Lwt.t
      (* [sleep d] is a threads which remain suspended for [d] seconds
         (letting other threads run) and then terminates. *)
val yield : unit -> unit Lwt.t
      (* [yield ()] is a threads which suspends itself (letting other
         thread run) and then resumes as soon as possible and
         terminates. *)

val run : 'a Lwt.t -> 'a
      (* [run t] lets the thread [t] run until it terminates.  It
         evaluates to the return value of [t], or raise the exception
         associated to [t] if [t] fails.

         You should avoid using [run] inside threads:
         - The calling threads will not resume before [run]
           returns.
         - Successive invocations of [run] are serialized: an
           invocation of [run] will not terminate before all
           subsequent invocations are terminated. *)

(****)

(* These functions behaves as their [Unix] counterparts, but let other
   threads run while waiting for the completion of the system call.

   PITFALL
   If you want to read or write from stdin, stdout or stderr using
   this library, you must first turn them into non-blocking mode
   using [Unix.set_nonblock]. *)

type file_descr

val of_unix_file_descr : Unix.file_descr -> file_descr

val read : file_descr -> string -> int -> int -> int Lwt.t
val write : file_descr -> string -> int -> int -> int Lwt.t
val wait_read : file_descr -> unit Lwt.t
val wait_write : file_descr -> unit Lwt.t
val pipe_in : unit -> file_descr * Unix.file_descr
val pipe_out : unit -> Unix.file_descr * file_descr
val socket :
  Unix.socket_domain -> Unix.socket_type -> int -> file_descr
val socketpair :
  Unix.socket_domain -> Unix.socket_type -> int ->
  (file_descr * file_descr) Lwt.t
val bind : file_descr -> Unix.sockaddr -> unit
val setsockopt : file_descr -> Unix.socket_bool_option -> bool -> unit
val accept : file_descr -> (file_descr * Unix.sockaddr) Lwt.t
val connect : file_descr -> Unix.sockaddr -> unit Lwt.t
val listen : file_descr -> int -> unit
val close : file_descr -> unit
val set_close_on_exec : file_descr -> unit

val wait : unit -> (int * Unix.process_status) Lwt.t
val waitpid : Unix.wait_flag list -> int -> (int * Unix.process_status) Lwt.t

val system : string -> Unix.process_status Lwt.t

type lwt_in_channel
type lwt_out_channel

val intern_in_channel : in_channel -> lwt_in_channel
val intern_out_channel : out_channel -> lwt_out_channel

val input_char : lwt_in_channel -> char Lwt.t
val input_line : lwt_in_channel -> string Lwt.t
val input : lwt_in_channel -> string -> int -> int -> int Lwt.t
val really_input : lwt_in_channel -> string -> int -> int -> unit Lwt.t

val open_process_in: string -> lwt_in_channel Lwt.t
val open_process_out: string -> lwt_out_channel Lwt.t
val open_process: string -> (lwt_in_channel * lwt_out_channel) Lwt.t
val open_process_full:
  string -> string array ->
  (lwt_in_channel * lwt_out_channel * lwt_in_channel) Lwt.t
val close_process_in: lwt_in_channel -> Unix.process_status Lwt.t
val close_process_out: lwt_out_channel -> Unix.process_status Lwt.t
val close_process:
  lwt_in_channel * lwt_out_channel -> Unix.process_status Lwt.t
val close_process_full:
  lwt_in_channel * lwt_out_channel * lwt_in_channel ->
  Unix.process_status Lwt.t
