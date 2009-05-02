(* Unison file synchronizer: src/remote.mli *)
(* Copyright 1999-2009, Benjamin C. Pierce (see COPYING for details) *)

module Thread : sig
  val unwindProtect : (unit -> 'a Lwt.t) -> (exn -> unit Lwt.t) -> 'a Lwt.t
end

(* Register a server function.  The result is a function that takes a host
   name as argument and either executes locally or else communicates with a
   remote server, as appropriate.  (Calling registerServerCmd also has the
   side effect of registering the command under the given name, so that when
   we are running as a server it can be looked up and executed when
   requested by a remote client.) *)
val registerHostCmd :
    string              (* command name *)
 -> ('a -> 'b Lwt.t)    (* local command *)
 -> (   string          (* -> host *)
     -> 'a              (*    arguments *)
     -> 'b Lwt.t)       (*    -> (suspended) result *)

(* A variant of registerHostCmd, for constructing a remote command to be
   applied to a particular root (host + fspath).
 -

   A naming convention: when a `root command' is built from a
   corresponding `local command', we name the two functions
   <funcName>OnRoot and <funcName>Local *)
val registerRootCmd :
    string                         (* command name *)
 -> ((Fspath.t * 'a) -> 'b Lwt.t)  (* local command *)
 -> (   Common.root                (* -> root *)
     -> 'a                         (*    additional arguments *)
     -> 'b Lwt.t)                  (*    -> (suspended) result *)

(* Enter "server mode", reading and processing commands from a remote
   client process until killed *)
val beAServer : unit -> unit
val waitOnPort : string option -> string -> unit

(* Whether the server should be killed when the client terminates *)
val killServer : bool Prefs.t

(* Establish a connection to the remote server (if any) corresponding
   to the root and return the canonical name of the root *)
val canonizeRoot :
  string -> Clroot.clroot -> (string -> string -> string) option ->
  Common.root Lwt.t

(* Statistics *)
val emittedBytes : float ref
val receivedBytes : float ref

(* Establish a connection to the server.
   First call openConnectionStart, then loop:
     call openConnectionPrompt, if you get a prompt,
     respond with openConnectionReply if desired.
   After you get None from openConnectionPrompt,
   call openConnectionEnd.
   Call openConnectionCancel to abort the connection.
*)
type preconnection
val openConnectionStart : Clroot.clroot -> preconnection option
val openConnectionPrompt : preconnection -> string option
val openConnectionReply : preconnection -> string -> unit
val openConnectionEnd : preconnection -> unit
val openConnectionCancel : preconnection -> unit

(* return the canonical name of the root.  The connection
   to the root must have already been established by
   the openConnection sequence. *)
val canonize : Clroot.clroot -> Common.root

(****)

type msgId = int
module MsgIdMap : Map.S with type key = msgId
val newMsgId : unit -> msgId

type connection
val connectionToRoot : Common.root -> connection

val registerServerCmd :
  string -> (connection -> 'a -> 'b Lwt.t) -> connection -> 'a -> 'b Lwt.t
val registerSpecialServerCmd :
  string ->
  ('a -> (string * int * int) list -> (string * int * int) list * int) *
  (string -> int -> 'a) ->
  ('b -> (string * int * int) list -> (string * int * int) list * int) *
  (string -> int -> 'b) ->
  (connection -> 'a -> 'b Lwt.t) -> connection -> 'a -> 'b Lwt.t
val defaultMarshalingFunctions :
  ('a -> (string * int * int) list -> (string * int * int) list * int) *
  (string -> int -> 'b)
val encodeInt : int -> string
val decodeInt : string -> int
val registerRootCmdWithConnection :
    string                          (* command name *)
 -> (connection -> 'a -> 'b Lwt.t)  (* local command *)
 ->    Common.root                  (* root on which the command is executed *)
    -> Common.root                  (* other root *)
    -> 'a                           (* additional arguments *)
    -> 'b Lwt.t                     (* result *)
