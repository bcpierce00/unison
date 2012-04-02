(* Module [Lwt]: cooperative light-weight threads. *)

type 'a t
      (* The type of threads returning a result of type ['a]. *)

val return : 'a -> 'a t
      (* [return e] is a thread whose return value is the value of
         the expression [e]. *)

val fail : exn -> 'a t
      (* [fail e] is a thread that fails with the exception [e]. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
      (* [bind t f] is a thread which first waits for the thread [t]
         to terminate and then, if the thread succeeds, behaves as the
         application of function [f] to the return value of [t].  If
         the thread [t] fails, [bind t f] also fails, with the same
         exception.

         The expression [bind t (fun x -> t')] can intuitively be read
         as [let x = t in t'].

         Note that [bind] is also often used just for synchronization
         purpose: [t'] will not execute before [t] is terminated.

         The result of a thread can be bound several time. *)

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      (* [t >>= f] is an alternative notation for [bind t f]. *)

val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
      (* [catch t f] is a thread that behaves as the thread [t ()] if
         this thread succeeds.  If the thread [t ()] fails with some
         exception, [catch t f] behaves as the application of [f] to
         this exception. *)

val try_bind : (unit -> 'a t) -> ('a -> 'b t) -> (exn -> 'b t) -> 'b t
     (* [try_bind t f g] behaves as [bind (t ()) f] if [t] does not fail.
        Otherwise, it behaves as the application of [g] to the
        exception associated to [t ()]. *)

val choose : 'a t list -> 'a t
      (* [choose l] behaves as the first thread in [l] to terminate.
         If several threads are already terminated, one is choosen
         at random. *)

val ignore_result : 'a t -> unit
      (* [ignore_result t] start the thread [t] and ignores its result
         value if the thread terminates sucessfully.  However, if the
         thread [t] fails, the exception is raised instead of being
         ignored.
         You should use this function if you want to start a thread
         and don't care what its return value is, nor when it
         terminates (for instance, because it is looping).
         Note that if the thread [t] yields and later fails, the
         exception will not be raised at this point in the program. *)

val wait : unit -> 'a t
      (* [wait ()] is a thread which sleeps forever (unless it is
         resumed by one of the functions [wakeup], [wakeup_exn] below).
         This thread does not block the execution of the remainder of
         the program (except of course, if another thread tries to
         wait for its termination). *)

(* Execution order
     A thread executes as much as possible.  Switching to another
     thread is always explicit.

   Exception handling
     - You must use "fail e" instead of "raise e" if you want the
       exception to be wrapped into the thread.
     - The construction [try t with ...] will not caught the
       exception associated to the thread [t] if this thread fails.
       You should use [catch] instead. *)

(****)

(* The functions below are probably not useful for the casual user.
   They provide the basic primitives on which can be built multi-
   threaded libraries such as Lwt_unix. *)

val poll : 'a t -> 'a option
      (* [poll e] returns [Some v] if the thread [e] is terminated and
         returned the value [v].  If the thread failed with some
         exception, this exception is raised.  If the thread is still
         running, [poll e] returns [None] without blocking. *)

val wakeup : 'a t -> 'a -> unit
      (* [wakeup t e] makes the sleeping thread [t] terminate and
         return the value of the expression [e]. *)
val wakeup_exn : 'a t -> exn -> unit
      (* [wakeup_exn t e] makes the sleeping thread [t] fail with the
         exception [e]. *)

val apply : ('a -> 'b t) -> 'a -> 'b t
      (* [apply f e] apply the function [f] to the expression [e].  If
         an exception is raised during this application, it is caught
         and the resulting thread fails with this exception. *)
(* Q: Could be called 'glue' or 'trap' or something? *)
