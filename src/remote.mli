(* Unison file synchronizer: src/remote.mli *)
(* Copyright 1999-2020, Benjamin C. Pierce (see COPYING for details) *)

module Thread : sig
  val unwindProtect : (unit -> 'a Lwt.t) -> (exn -> unit Lwt.t) -> 'a Lwt.t
end

(* A pair of functions enabling conversion from type 'a to a 2.51-compatible
   type and the other way around.
   The conversion functions are needed because the 2.51-compatible types must
   be frozen in time and never changed in future. Type 'a can and will change
   in time as enhancements are added and old code is removed.
   When a type is changed, breaking compatibility with 2.51, then respective
   conversion functions must also be added. *)
type 'a convV0Fun
val makeConvV0FunArg :
    ('a -> 'compat)
 -> ('compat -> 'a)
 -> 'a convV0Fun * 'b convV0Fun
val makeConvV0FunRet :
    ('b -> 'compat)
 -> ('compat -> 'b)
 -> 'a convV0Fun * 'b convV0Fun
val makeConvV0Funs :
    ('a -> 'compata)
 -> ('compata -> 'a)
 -> ('b -> 'compatb)
 -> ('compatb -> 'b)
 -> 'a convV0Fun * 'b convV0Fun

(* Register a server function.  The result is a function that takes a host
   name as argument and either executes locally or else communicates with a
   remote server, as appropriate.  (Calling registerServerCmd also has the
   side effect of registering the command under the given name, so that when
   we are running as a server it can be looked up and executed when
   requested by a remote client.) *)
val registerHostCmd :
    string              (* command name *)
 -> ?convV0: 'a convV0Fun * 'b convV0Fun
                        (* 2.51-compatibility functions for args and result *)
 -> 'a Umarshal.t -> 'b Umarshal.t
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
 -> ?convV0: (Fspath.t * 'a) convV0Fun * 'b convV0Fun
                                   (* 2.51-compatibility functions for args
                                      and result *)
 -> 'a Umarshal.t -> 'b Umarshal.t
 -> ((Fspath.t * 'a) -> 'b Lwt.t)  (* local command *)
 -> (   Common.root                (* -> root *)
     -> 'a                         (*    additional arguments *)
     -> 'b Lwt.t)                  (*    -> (suspended) result *)

(* Test whether a command exits on some root *)
val commandAvailable :
  Common.root ->                   (* root *)
  string ->                        (* command name *)
  bool Lwt.t

(* Enter "server mode", reading and processing commands from a remote
   client process until killed *)
val beAServer : unit -> unit
val waitOnPort : string list -> string -> unit

(* Whether the server should be killed when the client terminates *)
val killServer : bool Prefs.t

(* Establish a connection to the remote server (if any) corresponding
   to the root and return the canonical name of the root *)
val canonizeRoot :
  string -> Clroot.clroot -> (string -> string -> string) option ->
  Common.root Lwt.t

(* Test if connection to the remote server (if any) corresponding
   to the root is established. Always returns true for local roots *)
val isRootConnected : Common.root -> bool

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
val connectionVersion : connection -> int
val connectionToRoot : Common.root -> connection

val registerServerCmd :
  string
 -> ?convV0: 'a convV0Fun * 'b convV0Fun
 -> 'a Umarshal.t -> 'b Umarshal.t
 -> (connection -> 'a -> 'b Lwt.t)
 -> connection -> 'a -> 'b Lwt.t
val intSize : int
val encodeInt : int -> Bytearray.t * int * int
val decodeInt : Bytearray.t -> int -> int
val registerRootCmdWithConnection :
    string                          (* command name *)
 -> ?convV0: 'a convV0Fun * 'b convV0Fun
                                    (* 2.51-compatibility functions for args
                                       and result *)
 -> 'a Umarshal.t -> 'b Umarshal.t
 -> (connection -> 'a -> 'b Lwt.t)  (* local command *)
 ->    Common.root                  (* root on which the command is executed *)
    -> Common.root                  (* other root *)
    -> 'a                           (* additional arguments *)
    -> 'b Lwt.t                     (* result *)

val streamingActivated : bool Prefs.t

val registerStreamCmd :
  string ->
  (connection -> 'a ->
   (Bytearray.t * int * int) list -> (Bytearray.t * int * int) list * int) *
  (connection -> Bytearray.t -> int -> 'a) ->
  (connection -> 'a -> unit) ->
  connection -> (('a -> unit Lwt.t) -> 'b Lwt.t) -> 'b Lwt.t

(* Register a function to be run when the connection between client and server
   is closed. The function should not raise exceptions. If it does then running
   some of the other registered functions may be skipped (which is not an issue
   as the exception is likely going to quit the process).

   Registered functions are only expected to be run when the connection is
   closed but the process keeps running (a socket server, for example). Do not
   use it as a substitute for [at_exit]. *)
val at_conn_close : ?only_server:bool -> (unit -> unit) -> unit
