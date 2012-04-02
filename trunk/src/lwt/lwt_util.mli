
val join : unit Lwt.t list -> unit Lwt.t
    (* [join l] wait for all threads in [l] to terminate.
       If fails if one of the threads fail. *)

(****)

val iter : ('a -> unit Lwt.t) -> 'a list -> unit Lwt.t
    (* [iter f l] start a thread for each element in [l].  The threads
       are started according to the list order, but then can run
       concurrently.  It terminates when all the threads are
       terminated, if all threads are successful.  It fails if any of
       the threads fail. *)

val map : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
    (* [map f l] apply [f] to each element in [l] and collect the
       results of the threads thus created.  The threads are started
       according to the list order, but then can run concurrently.
       [map f l] fails if any of the threads fail. *)

val map_with_waiting_action :
    ('a -> 'b Lwt.t) -> ('a -> unit) -> 'a list -> 'b list Lwt.t
    (* [map_with_waiting_action f wa l] apply [f] to each element   *)
    (* in [l] and collect the results of the threads thus created.  *)
    (* The threads are started according to the list order, but     *)
    (* then can run concurrently.  The difference with [map f l] is *)
    (* that function wa will be called on the element that the      *)
    (* function is waiting for its termination.                     *)

val map_serial : ('a -> 'b Lwt.t) -> 'a list -> 'b list Lwt.t
    (* Similar to [map] but wait for one thread to terminate before
       starting the next one. *)

(****)

type region

val make_region : int -> region
      (* [make_region sz] create a region of size [sz]. *)
val resize_region : region -> int -> unit
      (* [resize_region reg sz] resize the region [reg] to size [sz]. *)
val run_in_region : region -> int -> (unit -> 'a Lwt.t) -> 'a Lwt.t
      (* [run_in_region reg size f] execute the thread produced by the
         function [f] in the region [reg]. The thread is not started
         before some room is available in the region. *)
