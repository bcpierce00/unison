(* Unison file synchronizer: src/remote.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

let (>>=) = Lwt.bind

let debug = Trace.debug "remote"
let debugV = Trace.debug "remote_emit+"
let debugE = Trace.debug "remote+"
let debugT = Trace.debug "remote+"

(* BCP: The previous definitions of the last two were like this:
     let debugE = Trace.debug "remote_emit"
     let debugT = Trace.debug "thread"
   But that resulted in huge amounts of output from '-debug all'.
*)

let _ =
  if Sys.unix || Sys.cygwin then
    ignore(Sys.set_signal Sys.sigpipe Sys.Signal_ignore)

(*
   Flow-control mechanism (only active under Windows).
   Only one side is allowed to send messages at any given time.
   Once it has finished sending messages, a special message is sent
   meaning that the destination is now allowed to send messages.

   Threads behave in a very controlled way: they only perform possibly
   blocking I/Os through the remote module, and never call
   Lwt_unix.yield.  This mean that when one side gives up its right to
   write, we know that no matter how long we wait, it will not have
   anything to write.  This ensures that there is no deadlock.
   A more robust protocol would be to give up write permission
   whenever idle (not just after having sent at least one message).
   But then, there is the risk that the two sides exchange spurious
   messages.
*)

(****)

let intSize = 5

let intHash x = ((x * 791538121) lsr 23 + 17) land 255

let encodeInt m =
  let int_buf = Bytearray.create intSize in
  int_buf.{0} <- Char.chr ( m         land 0xff);
  int_buf.{1} <- Char.chr ((m lsr 8)  land 0xff);
  int_buf.{2} <- Char.chr ((m lsr 16) land 0xff);
  int_buf.{3} <- Char.chr ((m lsr 24) land 0xff);
  int_buf.{4} <- Char.chr (intHash m);
  (int_buf, 0, intSize)

let decodeInt int_buf i =
  let b0 = Char.code (int_buf.{i + 0}) in
  let b1 = Char.code (int_buf.{i + 1}) in
  let b2 = Char.code (int_buf.{i + 2}) in
  let b3 = Char.code (int_buf.{i + 3}) in
  let m = (b3 lsl 24) lor (b2 lsl 16) lor (b1 lsl 8) lor b0 in
  if Char.code (int_buf.{i + 4}) <> intHash m then
    raise (Util.Fatal
             "Protocol error: corrupted message received;\n\
              if it happens to you in a repeatable way, \n\
              please post a report on the unison-users mailing list.");
  m

(*************************************************************************)
(*                           LOW-LEVEL IO                                *)
(*************************************************************************)

let ioCleanups = ref []

let registerIOClose io f =
  ioCleanups := (io, f) :: !ioCleanups

let lostConnectionHandler ch =
  let aux ((i, o), f) =
    if i = ch || o = ch then begin
      f ();
      false (* Each handler is run only once *)
    end else
      true
  in
  ioCleanups := Safelist.filter aux !ioCleanups

let lostConnection ch =
  begin try lostConnectionHandler ch with _ -> () end;
  Lwt.fail (Util.Fatal "Lost connection with the server")

let catchIoErrors ch th =
  Lwt.catch th
    (fun e ->
       match e with
         Unix.Unix_error(Unix.ECONNRESET, _, _)
       | Unix.Unix_error(Unix.EPIPE, _, _)
       | Unix.Unix_error(Unix.ETIMEDOUT, _, _)
       | Unix.Unix_error(Unix.EACCES, _, _)        (* Linux firewall *)
         (* Windows may also return the following errors... *)
       | Unix.Unix_error(Unix.EINVAL, _, _) (* ... and Linux firewall *)
       | Unix.Unix_error(Unix.EUNKNOWNERR (-64), _, _)
                         (* ERROR_NETNAME_DELETED *)
       | Unix.Unix_error(Unix.EUNKNOWNERR (-233), _, _)
                         (* ERROR_PIPE_NOT_CONNECTED *)
       | Unix.Unix_error(Unix.EUNKNOWNERR (-1236), _, _)
                         (* ERROR_CONNECTION_ABORTED *)
         (* The following may indicate a programming error, but even if that's
            the case, let's be graceful about it rather than crash the server.
            (Seen happening on the socket server when the connection is broken
            before the client has read the updates sent by server.) *)
       | Unix.Unix_error(Unix.EBADF, _, _)
         (* The following errors _may_ be temporary but we don't know if
            they are or for how long they will persist. We also don't have
            a way to retry and there is no guarantee that the socket remains
            in a usable state, so treat all these as permanent failures
            breaking the connection. *)
       | Unix.Unix_error(Unix.ENETUNREACH, _, _)
       | Unix.Unix_error(Unix.EHOSTUNREACH, _, _)
       | Unix.Unix_error(Unix.ENETDOWN, _, _)
       | Unix.Unix_error(Unix.EHOSTDOWN, _, _)
       | Unix.Unix_error(Unix.ENETRESET, _, _) ->
         (* Client has closed its end of the connection *)
           lostConnection ch
       | _ ->
           Lwt.fail e)

(****)

let receivedBytes = ref 0.
let emittedBytes = ref 0.

(****)

(* I/O buffers *)

type ioBuffer =
  { channel : Lwt_unix.file_descr;
    buffer : bytes;
    mutable length : int;
    mutable opened : bool }

let bufferSize = 16384
(* No point in making this larger, as the Ocaml Unix library uses a
   buffer of this size *)

let makeBuffer ch =
  { channel = ch; buffer = Bytes.create bufferSize;
    length = 0; opened = true }

(****)

(* Low-level inputs *)

let fillInputBuffer conn =
  assert (conn.length = 0);
  catchIoErrors conn.channel
    (fun () ->
       Lwt_unix.read conn.channel conn.buffer 0 bufferSize >>= fun len ->
       debugV (fun() ->
         if len = 0 then
           Util.msg "grab: EOF\n"
         else
           Util.msg "grab: %s\n"
             (String.escaped (Bytes.sub_string conn.buffer 0 len)));
       if len = 0 then
         lostConnection conn.channel
       else begin
         receivedBytes := !receivedBytes +. float len;
         conn.length <- len;
         Lwt.return ()
       end)

let rec grabRec conn s pos len =
  if conn.length = 0 then begin
    fillInputBuffer conn >>= fun () ->
    grabRec conn s pos len
  end else begin
    let l = min (len - pos) conn.length in
    Bytearray.blit_from_bytes conn.buffer 0 s pos l;
    conn.length <- conn.length - l;
    if conn.length > 0 then
      Bytes.blit conn.buffer l conn.buffer 0 conn.length;
    if pos + l < len then
      grabRec conn s (pos + l) len
    else
      Lwt.return ()
  end

let grab conn s len =
  assert (len > 0);
  assert (Bytearray.length s <= len);
  grabRec conn s 0 len

let peekWithoutBlocking conn =
  Bytes.sub conn.buffer 0 conn.length

let peekWithBlocking conn =
  (if conn.length = 0 then begin
    fillInputBuffer conn
  end else
    Lwt.return ()) >>= fun () ->
  Lwt.return (peekWithoutBlocking conn)

(****)

(* Low-level outputs *)

let rec sendOutput conn =
  catchIoErrors conn.channel
    (fun () ->
       begin if conn.opened then
         Lwt_unix.write conn.channel conn.buffer 0 conn.length
       else
         Lwt.return conn.length
       end >>= fun len ->
       debugV (fun() ->
         Util.msg "dump: %s\n"
           (String.escaped (Bytes.sub_string conn.buffer 0 len)));
       emittedBytes := !emittedBytes +. float len;
       conn.length <- conn.length - len;
       if conn.length > 0 then
         Bytes.blit
           conn.buffer len conn.buffer 0 conn.length;
       Lwt.return ())

let rec fillBuffer2 conn s pos len =
  if conn.length = bufferSize then
    sendOutput conn >>= fun () ->
    fillBuffer2 conn s pos len
  else begin
    let l = min (len - pos) (bufferSize - conn.length) in
    Bytearray.blit_to_bytes s pos conn.buffer conn.length l;
    conn.length <- conn.length + l;
    if pos + l < len then
      fillBuffer2 conn s (pos + l) len
    else
      Lwt.return ()
  end

let rec fillBuffer conn l =
  match l with
    (s, pos, len) :: rem ->
      assert (pos >= 0);
      assert (len >= 0);
      assert (pos <= Bytearray.length s - len);
      fillBuffer2 conn s pos len >>= fun () ->
      fillBuffer conn rem
  | [] ->
      Lwt.return ()

let rec flushBuffer conn =
  if conn.length > 0 then
    sendOutput conn >>= fun () ->
    flushBuffer conn
  else
    Lwt.return ()

(****)

(* Output scheduling *)

type kind = Normal | Idle | Last | Urgent

type outputQueue =
  { mutable available : bool;
    mutable canWrite : bool;
    mutable flowControl : bool;
    writes : (kind * (unit -> unit Lwt.t) * unit Lwt.t) Queue.t;
    urgentWrites : (kind * (unit -> unit Lwt.t) * unit Lwt.t) Queue.t;
    idleWrites : (kind * (unit -> unit Lwt.t) * unit Lwt.t) Queue.t;
    flush : outputQueue -> unit Lwt.t }

let rec performOutputRec q (kind, action, res) =
  action () >>= fun () ->
  Lwt.wakeup res ();
  popOutputQueues q

and popOutputQueues q =
  if not (Queue.is_empty q.urgentWrites) then
    performOutputRec q (Queue.take q.urgentWrites)
  else if not (Queue.is_empty q.writes) && q.canWrite then
    performOutputRec q (Queue.take q.writes)
  else if not (Queue.is_empty q.idleWrites) && q.canWrite then
    performOutputRec q (Queue.take q.idleWrites)
  else begin
    q.available <- true;
    (* Flush asynchronously the output *)
    Lwt.ignore_result (q.flush q);
    Lwt.return ()
  end

(* Perform an output action in an atomic way *)
let performOutput q kind action =
  if q.available && (kind = Urgent || q.canWrite) then begin
    q.available <- false;
    performOutputRec q (kind, action, Lwt.wait ())
  end else begin
    let res = Lwt.wait () in
    Queue.add (kind, action, res)
      (match kind with
         Urgent -> q.urgentWrites
       | Normal -> q.writes
       | Idle   -> q.idleWrites
       | Last   -> assert false);
    res
  end

let allowWrites q =
  assert (not q.canWrite);
  q.canWrite <- true;
  q.available <- false;
  (* We yield to let the receiving thread restart and to let some time
     to the requests to be processed *)
  Lwt.ignore_result (Lwt_unix.yield () >>= fun () -> popOutputQueues q)

let disableFlowControl q =
  q.flowControl <- false;
  if not q.canWrite then allowWrites q

let outputQueueIsEmpty q = q.available

(* Setup IO with flow control initially disabled, to do the RPC version
   handshake. Flow control is part of RPC protocol and must be enabled
   only after RPC version handshake is complete. *)
let makeOutputQueue isServer flush =
  { available = true; canWrite = true; flowControl = false;
    writes = Queue.create (); urgentWrites = Queue.create ();
    idleWrites = Queue.create ();
    flush = flush }

(****)

(* IMPORTANT: the RPC version must be increased when the RPC mechanism itself
   changes in a breaking way. Changes on the API level (functions and data
   types) normally do not cause a breaking change at the RPC level. *)
(* Version 0 is special in that it must not be listed as a supported version.
   It is used for 2.51-compatibility mode and is never negotiated. *)
(* Supported RPC versions should be ordered from newest to oldest. *)
let rpcSupportedVersions = [1]
let rpcDefaultVersion = Safelist.hd rpcSupportedVersions

let rpcSupportedVersionStr =
  String.concat ", "
    (Safelist.map (fun v -> "\"" ^ string_of_int v ^ "\"")
       rpcSupportedVersions)

let rpcSupportedVersionStrHdr =
  String.concat " "
    (Safelist.map (fun v -> string_of_int v)
       rpcSupportedVersions)

(* FIX: Added in 2021. Should be removed after a couple of years. *)
let rpcServerCmdlineOverride = "__new-rpc-mode"

(****)

type connection =
  { mutable version : int;
    inputBuffer : ioBuffer;
    outputBuffer : ioBuffer;
    outputQueue : outputQueue }

let maybeFlush pendingFlush q buf =
  (* We return immediately if a flush is already scheduled, or if the
     output buffer is already empty. *)
  (* If we are doing flow control and we can write, we need to send
     a write token even when the buffer is empty. *)
  if
    !pendingFlush || (buf.length = 0 && not (q.flowControl && q.canWrite))
  then
    Lwt.return ()
  else begin
    pendingFlush := true;
    (* Wait a bit, in case there are some new requests being processed *)
    Lwt_unix.yield () >>= fun () ->
    pendingFlush := false;
    (* If there are other writes scheduled, we do not flush yet *)
    if outputQueueIsEmpty q then begin
      performOutput q Last
        (fun () ->
           if q.flowControl then begin
             debugE (fun() -> Util.msg "Sending write token\n");
             q.canWrite <- false;
             fillBuffer buf [encodeInt 0] >>= fun () ->
             flushBuffer buf
           end else
             flushBuffer buf) >>= fun () ->
      Lwt.return ()
    end else
      Lwt.return ()
  end

let makeConnection isServer inCh outCh =
  let pendingFlush = ref false in
  let outputBuffer = makeBuffer outCh in
  { version = rpcDefaultVersion;
    inputBuffer = makeBuffer inCh;
    outputBuffer = outputBuffer;
    outputQueue =
      makeOutputQueue isServer
        (fun q -> maybeFlush pendingFlush q outputBuffer) }

let closeConnection conn =
  begin try Lwt_unix.close conn.inputBuffer.channel with Unix.Unix_error _ -> () end;
  begin try Lwt_unix.close conn.outputBuffer.channel with Unix.Unix_error _ -> () end;
  conn.outputBuffer.opened <- false

let connectionIO conn =
  (conn.inputBuffer.channel, conn.outputBuffer.channel)

let setConnectionVersion conn ver =
  conn.version <- ver

let connectionVersion conn = conn.version

let connEq conn conn' =
  conn.inputBuffer.channel = conn'.inputBuffer.channel
    && conn.outputBuffer.channel = conn'.outputBuffer.channel

let connNeq conn conn' = not (connEq conn conn')

(* Send message [l] *)
let dump conn l =
  performOutput
    conn.outputQueue Normal (fun () -> fillBuffer conn.outputBuffer l)

(* Send message [l] when idle *)
let dumpIdle conn l =
  performOutput
    conn.outputQueue Idle (fun () -> fillBuffer conn.outputBuffer l)

(* Send message [l], even if write are disabled.  This is used for
   aborting rapidly a stream.  This works as long as only one small
   message is written at a time (the write will succeed as the pipe
   will not be full) *)
let dumpUrgent conn l =
  performOutput conn.outputQueue Urgent
    (fun () ->
       fillBuffer conn.outputBuffer l >>= fun () ->
       flushBuffer conn.outputBuffer)

let enableFlowControl conn isServer =
  let rec waitDrain () =
    if not isServer && conn.outputBuffer.length > 0 then
      Lwt_unix.yield () >>= waitDrain
    else
      Lwt.return ()
  in
  let q = conn.outputQueue in
  q.available <- false;
  waitDrain () >>= fun () ->
  q.flowControl <- true;
  q.canWrite <- isServer;
  if q.canWrite then
    popOutputQueues q >>= Lwt_unix.yield >>= fun () ->
    Lwt.return ()
  else
    Lwt.return ()

(****)

let connectionCheck = ref None

let checkConnection ioServer =
  connectionCheck := Some ioServer;
  (* Poke on the socket to trigger an error if connection has been lost. *)
  Lwt_unix.run (
    (if Sys.win32 then Lwt.return 0 else
    Lwt_unix.read ioServer.inputBuffer.channel ioServer.inputBuffer.buffer 0 0)
    (* Try to make sure connection cleanup, if necessary, has finished
       before returning.
       Since there is no way to reliably detect when other threads have
       finished, we just yield a bit (the same comments apply as in
       commandLoop). *)
    >>= fun _ ->
    let rec wait n =
      if n = 0 then Lwt.return () else begin
        Lwt_unix.yield () >>= fun () ->
        wait (n - 1)
      end
    in
    wait 10);
  connectionCheck := None

let isConnectionCheck conn =
  match !connectionCheck with
  | None -> false
  | Some conn' -> connEq conn conn'

(* Due to [Common.root] currently excluding important details present in
   [Clroot.clroot], there is a 1:N mapping between a [root] and a [clroot].
   For example, a [clroot] pointing to the same host but a different
   protocol or user or port will be mapped to the same [root] as long as
   the root fspaths are the same.
   It is currently (Oct 2022) not seen as a critical issue to fix.
   The code previously used to index connections just by the canonical host
   name, ignoring all other details, including the root fspath. That code
   was in place for over 20 years and did not seem to cause any issues.
   The current code is safer than the previous code... *)
module ClientConn = struct
  type t =
    { clroot : Clroot.clroot;
      root : Common.root;
      conn : connection }
      (* Never do polymorphic comparisons with [connection]! *)

  (* The replica path of a root is not a relevant detail for a connection;
     on the contrary, it must not impact the connection. Instead of creating
     a new type for connection tracking, for now, just exclude the path in
     clroot comparisons. Eventually, this could/should be solved in a better
     way: see the comment above about [Common.root] not being sufficiently
     detailed. *)
  let clrootEq clroot clroot' =
    match clroot, clroot' with
    | Clroot.ConnectByShell (shell, host, user, port, _),
      Clroot.ConnectByShell (shell', host', user', port', _) ->
        shell = shell' && host = host' && user = user' && port = port'
    | ConnectBySocket (host, port, _),
      ConnectBySocket (host', port', _) ->
        host = host' && port = port'
    | ConnectByShell _, _ | _, ConnectByShell _ -> false
    | ConnectBySocket _, _ | _, ConnectBySocket _ -> false
    | ConnectLocal _, ConnectLocal _ -> assert false

  let clrootNeq clroot clroot' = not (clrootEq clroot clroot')

  let connections = ref []

  let findByClroot clroot =
    Safelist.find (fun x -> clrootEq x.clroot clroot) !connections

  let findByRoot root = Safelist.find (fun x -> x.root = root) !connections

  let register clroot root conn =
    connections := { clroot; root; conn } ::
      Safelist.filter (fun x -> clrootNeq x.clroot clroot) !connections

  let unregister conn =
    connections := Safelist.filter (fun x -> connNeq x.conn conn) !connections

  let ofRoot root =
    try (findByRoot root).conn with
    | Not_found -> raise (Util.Fatal "No connection with the server")

  let ofRootOpt root =
    try Some (findByRoot root).conn with
    | Not_found -> None

  let canonRootOfClroot clroot =
    try Some (findByClroot clroot).root with
    | Not_found -> None

  let withConncheck find =
    try
      let conn = (find ()).conn in
      begin try
        checkConnection conn
      with
      | Unix.Unix_error (EBADF, _, _) -> (* Already closed *)
          unregister conn
        (* (All or most?) other exceptions should be caught by receiving and
           sending threads. If this does not happen (it also depends on the
           implementation of [Lwt_unix.run]) then trigger the cleanup here as
           the last resort. *)
      | Unix.Unix_error _ ->
          try
            lostConnection (fst (connectionIO conn)) |> ignore;
            unregister conn
          with e -> begin
            unregister conn;
            raise e
          end
      end;
      (* [find] _must_ be duplicated after [checkConnection]! *)
      Some (find ()).conn
    with Not_found ->
      None

  let ofRootConncheck root =
    withConncheck (fun () -> findByRoot root)

  let ofClrootConncheck clroot =
    withConncheck (fun () -> findByClroot clroot)

end (* module ClientConn *)

let connectionOfRoot root = ClientConn.ofRoot root

(****)

let atCloseHandlers = ref []

let at_conn_close ?(only_server = false) f =
  atCloseHandlers := (only_server, f) :: !atCloseHandlers

let runConnCloseHandlers isServer =
  Safelist.iter (fun (only_server, f) ->
    if not only_server || isServer then f ()) !atCloseHandlers

let atConnCloseHandlers = ref []

let at_conn_close' conn f =
  atConnCloseHandlers := (conn, f) :: !atConnCloseHandlers

let runConnCloseHandlers' conn =
  atConnCloseHandlers := Safelist.filter (fun (c, f) ->
    if connEq c conn then (f (); false) else true) !atConnCloseHandlers

let clientCloseCleanup () =
  runConnCloseHandlers false

let clientConnClose conn =
  closeConnection conn;
  ClientConn.unregister conn;
  runConnCloseHandlers' conn;
  clientCloseCleanup ()

let registerConnCleanup conn cleanup =
  registerIOClose (connectionIO conn) (fun () -> clientConnClose conn);
  match cleanup with
  | None -> ()
  | Some f -> at_conn_close' conn f

let clientCloseRootConnection = function
  | (Common.Local, _) -> clientCloseCleanup ()
  | (Common.Remote _, _) as root ->
      begin match ClientConn.ofRootOpt root with
      | Some conn -> clientConnClose conn
      | None -> ()
      end

(****)

(* Implemented as a record to avoid polluting [Remote] namespace. If
   the number and complexity of functions grows in future then it's
   probably a good idea to extract this code into a separate module. *)
type ('a, 'b, 'c) resourceC =
  { register : 'a -> 'a; release : 'a -> 'b; release_noerr : 'a -> 'c }
let resourceWithConnCleanup close close_noerr =
  let h = Hashtbl.create 17 in
  let closeAll () =
    Hashtbl.iter (fun x _ -> ignore (close_noerr x)) h;
    Hashtbl.clear h
  in
  at_conn_close closeAll;
  let register x = Hashtbl.add h x true; x in
  let release x = Hashtbl.remove h x; close x in
  let release_noerr x = Hashtbl.remove h x; close_noerr x in
  { register; release; release_noerr }

let lwtRegionWithConnCleanup sz =
  let reg = ref (Lwt_util.make_region sz) in
  let resetReg () =
    Lwt_util.purge_region !reg;
    (* The remaining threads should be collected by GC *)
    reg := Lwt_util.make_region sz
  in
  at_conn_close resetReg;
  reg

(****)

(* XXX *)
module Thread = struct

  let unwindProtect f cleanup =
    Lwt.catch f
      (fun e ->
         match e with
           Util.Transient err | Util.Fatal err ->
             debugT
               (fun () ->
                  Util.msg
                    "Exception caught by Thread.unwindProtect: %s\n" err);
             Lwt.catch (fun () -> cleanup e) (fun e' ->
               Util.encodeException "Thread.unwindProtect" `Fatal e')
                 >>= (fun () ->
             Lwt.fail e)
         | _ ->
             Lwt.fail e)

end

(*****************************************************************************)
(*                              MARSHALING                                   *)
(*****************************************************************************)

type tag = Bytearray.t

type 'a marshalFunction = connection ->
  'a -> (Bytearray.t * int * int) list -> (Bytearray.t * int * int) list
type 'a unmarshalFunction = connection -> Bytearray.t -> 'a
type 'a marshalingFunctions = 'a marshalFunction * 'a unmarshalFunction

type 'a convV0Fun =
  V0 : ('a -> 'compat) * ('compat -> 'a) -> 'a convV0Fun

external id : 'a -> 'a = "%identity"
let convV0_id = V0 (id, id)
let convV0_id_pair = convV0_id, convV0_id

let makeConvV0FunArg compat_to compat_from =
  (V0 (compat_to, compat_from)), convV0_id
let makeConvV0FunRet compat_to compat_from =
  convV0_id, (V0 (compat_to, compat_from))
let makeConvV0Funs compat_to compat_from compat_to2 compat_from2 =
  (V0 (compat_to, compat_from)), (V0 (compat_to2, compat_from2))

let registeredSet = ref Util.StringSet.empty

let rec first_chars len msg =
  match msg with
    [] ->
      ""
  | (s, p, l) :: rem ->
      if l < len then
        Bytearray.sub s p l ^ first_chars (len - l) rem
      else
        Bytearray.sub s p len

let safeMarshal marshalPayload tag data rem =
  let (rem', length) = marshalPayload data rem in
  let l = Bytearray.length tag in
  debugE (fun() ->
            let start = first_chars (min length 10) rem' in
            let start = if length > 10 then start ^ "..." else start in
            let start = String.escaped start in
            Util.msg "send [%s] '%s' %d bytes\n"
              (Bytearray.to_string tag) start length);
  let len = l + length in
  if (len lsr 31) lsr 1 <> 0 then (* [encodeInt] can only encode 32 bits *)
    raise (Util.Fatal
            "Protocol error: message data too big. This may be a bug or it\n\
             may be that your replicas are huge and the amount of updates\n\
             can't be handled by the current protocol implementation. If you\n\
             believe it is a bug then please consider reporting it.\n\
             Otherwise, try reducing the amount of updates by syncing the\n\
             replicas in smaller steps (using the \"path\" preference, for\n\
             example). You may have to do this for the initial sync only.")
  else
    (encodeInt len :: (tag, 0, l) :: rem')

let safeUnmarshal unmarshalPayload tag buf =
  let taglength = Bytearray.length tag in
  if Bytearray.prefix tag buf 0 then
    unmarshalPayload buf taglength
  else
    let identifier =
      String.escaped
        (Bytearray.sub buf 0 (min taglength (Bytearray.length buf))) in
    raise (Util.Fatal
             (Printf.sprintf "[safeUnmarshal] expected '%s' but got '%s'"
                (String.escaped (Bytearray.to_string tag)) identifier))

let registerTag string =
  if Util.StringSet.mem string !registeredSet then
    raise (Util.Fatal (Printf.sprintf "tag %s is already registered" string))
  else
    registeredSet := Util.StringSet.add string !registeredSet;
  Bytearray.of_string string

let marshalV0 (V0 (to251, _)) data rem =
  let s = Bytearray.marshal (to251 data) [Marshal.No_sharing] in
  let l = Bytearray.length s in
  ((s, 0, l) :: rem, l)

let unmarshalV0 (V0 (_, from251)) buf pos =
  try from251 (Bytearray.unmarshal buf pos)
  with Failure s -> raise (Util.Fatal (Printf.sprintf
"Fatal error during unmarshaling (%s),
possibly because client and server have been compiled with different \
versions of the OCaml compiler." s))

let marshalV1 m data rem =
  let s = Umarshal.marshal_to_bytearray m data in
  let l = Bytearray.length s in
  ((s, 0, l) :: rem, l)

let unmarshalV1 m buf pos =
  try Umarshal.unmarshal_from_bytearray m buf pos
  with Failure s | Umarshal.Error s -> raise (Util.Fatal (Printf.sprintf
"Fatal error during unmarshaling (%s)" s))

let defaultMarshalingFunctions convV0 m =
  (fun conn -> if conn.version = 0 then marshalV0 convV0 else marshalV1 m),
  (fun conn -> if conn.version = 0 then unmarshalV0 convV0 else unmarshalV1 m)

let makeMarshalingFunctions payloadMarshalingFunctions string =
  let (marshalPayload, unmarshalPayload) = payloadMarshalingFunctions in
  let tag = registerTag string in
  let marshal conn (data : 'a) rem = safeMarshal (marshalPayload conn) tag data rem in
  let unmarshal conn buf = (safeUnmarshal (unmarshalPayload conn) tag buf : 'a) in
  (marshal, unmarshal)

(*****************************************************************************)
(*                              SERVER SETUP                                 *)
(*****************************************************************************)

(* BCPFIX: Now that we've beefed up the clroot data structure, shouldn't
   these be part of it too? *)
let sshCmd =
  Prefs.createString "sshcmd" "ssh"
    ~category:(`Advanced `Remote)
    ("path to the ssh executable")
    ("This preference can be used to explicitly set the name of the "
     ^ "ssh executable (e.g., giving a full path name), if necessary.")

let sshargs =
  Prefs.createString "sshargs" ""
    ~category:(`Advanced `Remote)
    "other arguments (if any) for remote shell command"
    ("The string value of this preference will be passed as additional "
     ^ "arguments (besides the host name and the name of the Unison "
     ^ "executable on the remote system) to the \\verb|ssh| "
     ^ "command used to invoke the remote server. The backslash is an "
     ^ "escape character."
     )

(* rsh prefs removed since 2.52 *)
let () = Prefs.markRemoved "rshcmd"
let () = Prefs.markRemoved "rshargs"

let serverCmd =
  Prefs.createString "servercmd" ""
    ~category:(`Advanced `Remote)
    ("name of " ^ Uutil.myName ^ " executable on remote server")
    ("This preference can be used to explicitly set the name of the "
     ^ "Unison executable on the remote server (e.g., giving a full "
     ^ "path name), if necessary.")

let addversionno =
  Prefs.createBool "addversionno" false
    ~category:(`Advanced `Remote)
    ("add version number to name of " ^ Uutil.myName ^ " on server")
    ("When this flag is set to {\\tt true}, Unison "
     ^ "will use \\texttt{unison-\\ARG{currentmajorversionnumber}} instead of "
     ^ "just \\verb|unison| as the remote server command (note that the minor "
     ^ "version number is dropped -- e.g., unison-2.51).  This allows "
     ^ "multiple binaries for different versions of unison to coexist "
     ^ "conveniently on the same server: whichever version is run "
     ^ "on the client, the same version will be selected on the server.")

(**********************************************************************
                       CLIENT/SERVER PROTOCOLS
 **********************************************************************)

(*
Each protocol has a name, a client side, and a server side.

The server remembers the server side of each protocol in a table
indexed by protocol name. The function of the server is to wait for
the client to invoke a protocol, and carry out the appropriate server
side.

Protocols are invoked on the client with arguments for the server side.
The result of the protocol is the result of the server side. In types,

  serverSide : 'a -> 'b

That is, the server side takes arguments of type 'a from the client,
and returns a result of type 'b.

A protocol is started by the client sending a Request packet and then a
packet containing the protocol name to the server.  The server looks
up the server side of the protocol in its table.

Next, the client sends a packet containing marshaled arguments for the
server side.

The server unmarshals the arguments and invokes the server side with
the arguments from the client.

When the server side completes it gives a result. The server marshals
the result and sends it to the client.  (Instead of a result, the
server may also send back either a Transient or a Fatal error packet).
Finally, the client can receive the result packet from the server and
unmarshal it.

The protocol is fully symmetric, so the server may send a Request
packet to invoke a function remotely on the client.  In this case, the
two switch roles.)
*)

let receivePacket conn =
  (* Get the length of the packet *)
  let int_buf = Bytearray.create intSize in
  grab conn.inputBuffer int_buf intSize >>= (fun () ->
  let length = decodeInt int_buf 0 in
  assert (length >= 0);
  (* Get packet *)
  let buf = Bytearray.create length in
  grab conn.inputBuffer buf length >>= (fun () ->
  (debugE (fun () ->
             let start =
               if length > 10 then (Bytearray.sub buf 0 10) ^ "..."
               else Bytearray.sub buf 0 length in
             let start = String.escaped start in
             Util.msg "receive '%s' %d bytes\n" start length);
   Lwt.return buf)))

type servercmd =
  connection -> Bytearray.t ->
  ((Bytearray.t * int * int) list -> (Bytearray.t * int * int) list) Lwt.t
let serverCmds = ref (Util.StringMap.empty : servercmd Util.StringMap.t)

type serverstream =
  connection -> Bytearray.t -> unit
let serverStreams = ref (Util.StringMap.empty : serverstream Util.StringMap.t)

type header =
    NormalResult
  | TransientExn of string
  | FatalExn of string
  | Request of string
  | Stream of string
  | StreamAbort

let mheader = Umarshal.(sum6 unit string string string string unit
                          (function
                           | NormalResult -> I61 ()
                           | TransientExn a -> I62 a
                           | FatalExn a -> I63 a
                           | Request a -> I64 a
                           | Stream a -> I65 a
                           | StreamAbort -> I66 ())
                          (function
                           | I61 () -> NormalResult
                           | I62 a -> TransientExn a
                           | I63 a -> FatalExn a
                           | I64 a -> Request a
                           | I65 a -> Stream a
                           | I66 () -> StreamAbort))

let ((marshalHeader, unmarshalHeader) : header marshalingFunctions) =
  makeMarshalingFunctions (defaultMarshalingFunctions convV0_id mheader) "rsp"

let processRequest conn id cmdName buf =
  let cmd =
    try Util.StringMap.find cmdName !serverCmds
    with Not_found -> raise (Util.Fatal (cmdName ^ " not registered!"))
  in
  Lwt.try_bind (fun () -> cmd conn buf)
    (fun marshal ->
       debugE (fun () -> Util.msg "Sending result (id: %d)\n" (decodeInt id 0));
       dump conn ((id, 0, intSize) :: marshalHeader conn NormalResult (marshal [])))
    (function
       Util.Transient s ->
         debugE (fun () ->
           Util.msg "Sending transient exception (id: %d)\n" (decodeInt id 0));
         dump conn ((id, 0, intSize) :: marshalHeader conn (TransientExn s) [])
     | Util.Fatal s ->
         debugE (fun () ->
           Util.msg "Sending fatal exception (id: %d)\n" (decodeInt id 0));
         dump conn ((id, 0, intSize) :: marshalHeader conn (FatalExn s) [])
     | e ->
         Lwt.fail e)
  (* With the current RPC protocol it is not possible to recover from situations
     where an RPC packet is not completely transmitted (due to an interrupted
     write syscall). The other side will hang forever, waiting for the complete
     packet to arrive. New packets (here, transmitting the exception) can't be
     sent because the receiver can't read them until the previous packet is
     complete. (Best case, the server quits with a protocol error.)

     Therefore, it is important that exceptions that can interrupt syscalls
     (for example, Sys.Break (Ctrl-C)) are never wrapped into Util.Transient or
     Util.Fatal, unless the connection is already known to be broken. Likewise,
     other exception handlers than the one just above must avoid writing out
     additional data to the RPC connection. *)

let streamAbortedSrc = ref 0
let streamAbortedDst = ref false

let streamError = Hashtbl.create 7

let resetStreamErroState () =
  streamAbortedSrc := 0;
  streamAbortedDst := false;
  Hashtbl.reset streamError
let () = at_conn_close resetStreamErroState

let abortStream conn id =
  if not !streamAbortedDst then begin
    streamAbortedDst := true;
    let request = encodeInt id :: marshalHeader conn StreamAbort [] in
    dumpUrgent conn request
  end else
    Lwt.return ()

let processStream conn id cmdName buf =
  let id = decodeInt id 0 in
  if Hashtbl.mem streamError id then
   abortStream conn id
  else begin
    begin try
      let cmd =
        try Util.StringMap.find cmdName !serverStreams
        with Not_found -> raise (Util.Fatal (cmdName ^ " not registered!"))
      in
      cmd conn buf;
      Lwt.return ()
    with e ->
      Hashtbl.add streamError id e;
      abortStream conn id
    end
  end

(* Message ids *)
type msgId = int
module MsgIdMap = Map.Make (struct type t = msgId let compare = compare end)
(* An integer just a little smaller than the maximum representable in
   30 bits *)
let hugeint = 1000000000
let ids = ref 1
let newMsgId () = incr ids; if !ids = hugeint then ids := 2; !ids

(* Threads waiting for a response from the other side *)
let receivers = ref MsgIdMap.empty
let resetReceivers () =
  receivers := MsgIdMap.empty
let () = at_conn_close resetReceivers

let find_receiver id =
  let thr = MsgIdMap.find id !receivers in
  receivers := MsgIdMap.remove id !receivers;
  thr

(* Receiving thread: read a message and dispatch it to the right
   thread or create a new thread to process requests. *)
let rec receive conn =
  begin
    debugE (fun () -> Util.msg "Waiting for next message\n");
    (* Get the message ID *)
    let id = Bytearray.create intSize in
    grab conn.inputBuffer id intSize >>= (fun () ->
    let num_id = decodeInt id 0 in
    if num_id = 0 then begin
      debugE (fun () -> Util.msg "Received the write permission\n");
      allowWrites conn.outputQueue;
      receive conn
    end else begin
      debugE
        (fun () -> Util.msg "Message received (id: %d)\n" num_id);
      (* Read the header *)
      receivePacket conn >>= (fun buf ->
      let req = unmarshalHeader conn buf in
      begin match req with
        Request cmdName ->
          receivePacket conn >>= (fun buf ->
          (* We yield before starting processing the request.
             This way, the request may call [Lwt_unix.run] and this will
             not block the receiving thread. *)
          Lwt.ignore_result
            (Lwt_unix.yield () >>= (fun () ->
             processRequest conn id cmdName buf));
          receive conn)
      | NormalResult ->
          receivePacket conn >>= (fun buf ->
          Lwt.wakeup (find_receiver num_id) buf;
          receive conn)
      | TransientExn s ->
          debugV (fun() -> Util.msg "receive: Transient remote error '%s']" s);
          Lwt.wakeup_exn (find_receiver num_id) (Util.Transient s);
          receive conn
      | FatalExn s ->
          debugV (fun() -> Util.msg "receive: Fatal remote error '%s']" s);
          Lwt.wakeup_exn (find_receiver num_id) (Util.Fatal ("Server: " ^ s));
          receive conn
      | Stream cmdName ->
          receivePacket conn >>= fun buf ->
          processStream conn id cmdName buf >>= fun () ->
          receive conn
      | StreamAbort ->
          streamAbortedSrc := num_id;
          receive conn
      end)
    end)
  end

let wait_for_reply id =
  let res = Lwt.wait () in
  receivers := MsgIdMap.add id res !receivers;
  (* We yield to let the receiving thread restart.  This way, the
     thread may call [Lwt_unix.run] and this will not block the
     receiving thread. *)
  Lwt.catch
    (fun () ->
       res >>= (fun v -> Lwt_unix.yield () >>= (fun () -> Lwt.return v)))
    (fun e -> Lwt_unix.yield () >>= (fun () -> Lwt.fail e))

let registerSpecialServerCmd
    (cmdName : string)
    marshalingFunctionsArgs
    marshalingFunctionsResult
    (serverSide : connection -> 'a -> 'b Lwt.t)
    =
  (* Check that this command name has not already been bound *)
  if (Util.StringMap.mem cmdName !serverCmds) then
    raise (Util.Fatal (cmdName ^ " already registered!"));
  (* Create marshaling and unmarshaling functions *)
  let ((marshalArgs,unmarshalArgs) : 'a marshalingFunctions) =
    makeMarshalingFunctions marshalingFunctionsArgs (cmdName ^ "-args") in
  let ((marshalResult,unmarshalResult) : 'b marshalingFunctions) =
    makeMarshalingFunctions marshalingFunctionsResult (cmdName ^ "-res") in
  (* Create a server function and remember it *)
  let server conn buf =
    let args = unmarshalArgs conn buf in
    serverSide conn args >>= (fun answer ->
    Lwt.return (marshalResult conn answer))
  in
  serverCmds := Util.StringMap.add cmdName server !serverCmds;
  (* Create a client function and return it *)
  let client conn serverArgs =
    let id = newMsgId () in (* Message ID *)
    assert (id >= 0); (* tracking down an assert failure in receivePacket... *)
    let request =
      encodeInt id ::
      marshalHeader conn (Request cmdName) (marshalArgs conn serverArgs [])
    in
    let reply = wait_for_reply id in
    debugE (fun () -> Util.msg "Sending request (id: %d)\n" id);
    dump conn request >>= (fun () ->
    reply >>= (fun buf ->
    Lwt.return (unmarshalResult conn buf)))
  in
  client

let registerServerCmd name ?(convV0=convV0_id_pair) mArg mRet f =
  registerSpecialServerCmd
    name (defaultMarshalingFunctions (fst convV0) mArg)
         (defaultMarshalingFunctions (snd convV0) mRet) f

(* RegisterHostCmd is a simpler version of registerClientServer [registerServerCmd?].
   It is used to create remote procedure calls: the only communication
   between the client and server is the sending of arguments from
   client to server, and the sending of the result from the server
   to the client. Thus, server side does not need the file descriptors
   for communication with the client.

   RegisterHostCmd recognizes the case where the server is the local
   host, and it avoids socket communication in this case.
*)
let registerHostCmd cmdName ?(convV0=convV0_id_pair) mArg mRet cmd =
  let serverSide = (fun _ args -> cmd args) in
  let client0 =
    registerServerCmd cmdName ~convV0 mArg mRet serverSide in
  let client root args =
    let conn = ClientConn.ofRoot root in
    client0 conn args in
  (* Return a function that runs either the proxy or the local version,
     depending on whether the call is to the local host or a remote one *)
  fun root args ->
    match root with
    | (Common.Local, _) -> cmd args
    | (Common.Remote _, _) -> client root args

(* RegisterRootCmd is like registerHostCmd but it indexes connections by
   root instead of host. *)
let registerRootCmd (cmdName : string)
  ?(convV0=convV0_id_pair) mArg mRet (cmd : (Fspath.t * 'a) -> 'b) =
  let mArg = Umarshal.(prod2 Fspath.m mArg id id) in
  let r = registerHostCmd cmdName ~convV0 mArg mRet cmd in
  fun root args -> r root ((snd root), args)

let registerRootCmdWithConnection (cmdName : string)
  ?(convV0=convV0_id_pair) mArg mRet (cmd : connection -> 'a -> 'b) =
  let client0 = registerServerCmd cmdName ~convV0 mArg mRet cmd in
  (* Return a function that runs either the proxy or the local version,
     depending on whether the call is to the local host or a remote one *)
  fun localRoot remoteRoot args ->
    match (fst localRoot) with
    | Common.Local -> let conn = ClientConn.ofRoot remoteRoot in
            cmd conn args
    | _  -> let conn = ClientConn.ofRoot localRoot in
            client0 conn args

let streamReg = lwtRegionWithConnCleanup 1

let streamingActivated =
  Prefs.createBool "stream" true
    ~category:(`Advanced `Remote)
    ~deprecated:true
    ("use a streaming protocol for transferring file contents")
    "When this preference is set, Unison will use an experimental \
     streaming protocol for transferring file contents more efficiently. \
     The default value is \\texttt{true}."

let registerStreamCmd
    (cmdName : string)
    marshalingFunctionsArgs
    (serverSide : connection -> 'a -> unit)
    =
  let cmd =
    registerSpecialServerCmd
      cmdName marshalingFunctionsArgs
      (defaultMarshalingFunctions convV0_id Umarshal.unit)
      (fun conn v -> serverSide conn v; Lwt.return ())
  in
  let ping =
    registerServerCmd (cmdName ^ "Ping") Umarshal.int Umarshal.unit
      (fun conn (id : int) ->
         try
           let e = Hashtbl.find streamError id in
           Hashtbl.remove streamError id;
           streamAbortedDst := false;
           Lwt.fail e
         with Not_found ->
           Lwt.return ())
  in
  (* Check that this command name has not already been bound *)
  if (Util.StringMap.mem cmdName !serverStreams) then
    raise (Util.Fatal (cmdName ^ " already registered!"));
  (* Create marshaling and unmarshaling functions *)
  let ((marshalArgs,unmarshalArgs) : 'a marshalingFunctions) =
    makeMarshalingFunctions marshalingFunctionsArgs (cmdName ^ "-str") in
  (* Create a server function and remember it *)
  let server conn buf =
    let args = unmarshalArgs conn buf in
    serverSide conn args
  in
  serverStreams := Util.StringMap.add cmdName server !serverStreams;
  (* Create a client function and return it *)
  let client conn id serverArgs =
    debugE (fun () -> Util.msg "Sending stream chunk (id: %d)\n" id);
    if !streamAbortedSrc = id then raise (Util.Transient "Streaming aborted");
    let request =
      encodeInt id ::
      marshalHeader conn (Stream cmdName) (marshalArgs conn serverArgs [])
    in
    dumpIdle conn request
  in
  fun conn sender ->
    if not (Prefs.read streamingActivated) then
      sender (fun v -> cmd conn v)
    else begin
      (* At most one active stream at a time *)
      let id = newMsgId () in (* Message ID *)
      Lwt.try_bind
        (fun () ->
           Lwt_util.run_in_region !streamReg 1
             (fun () -> sender (fun v -> client conn id v)))
        (fun v -> ping conn id >>= fun () -> Lwt.return v)
        (fun e ->
           if !streamAbortedSrc = id then begin
             debugE (fun () ->
               Util.msg "Pinging remote end after streaming error\n");
             ping conn id >>= fun () -> Lwt.fail e
           end else
             Lwt.fail e)
    end

let commandAvailable =
  registerRootCmd "commandAvailable" Umarshal.string Umarshal.bool
    (fun (_, cmdName) -> Lwt.return (Util.StringMap.mem cmdName !serverCmds))

(****************************************************************************
                     BUILDING CONNECTIONS TO THE SERVER
 ****************************************************************************)

let receiveUntilSep ?(space=false) ?(nl=true) ?(includesep=false) conn =
  assert (space || nl);
  let inp = Buffer.create 32
  and buf = Bytearray.create 1 in
  let add () = Buffer.add_char inp buf.{0} in
  let rec aux () =
    grab conn.inputBuffer buf 1 >>= fun () ->
    match buf.{0} with
    | ' ' | '\t' when space ->
        if includesep then add (); Lwt.return (Buffer.contents inp)
    | '\n' when nl ->
        if includesep then add (); Lwt.return (Buffer.contents inp)
    | '\r' ->
        aux () (* ignore *)
    | _ ->
        add (); aux ()
  in aux ()

(* Get input until newline (excluded), blocking *)
let receiveUntilNewline conn =
  receiveUntilSep ~nl:true conn

(* Get input until space or newline, separator included by default; blocking *)
let receiveUntilSpaceOrNl ?(includesep=true) conn =
  receiveUntilSep ~space:true ~nl:true ~includesep conn

(* Get input of fixed length, blocking *)
let receiveString conn len =
  let buf = Bytearray.create len in
  grab conn.inputBuffer buf len >>= fun () ->
  Lwt.return (Bytearray.to_string buf)

(* Get input until newline (excluded), non-blocking *)
let receiveUntilNewlineNb conn =
  let e = Bytes.to_string (peekWithoutBlocking conn.inputBuffer) in
  let len = try String.index e '\n' with Not_found -> String.length e in
  receiveString conn len

let sendStrings conn slist =
  dump conn
    (Safelist.map (fun s -> (Bytearray.of_string s, 0, String.length s)) slist)

let sendString conn s = sendStrings conn [s]

(****)

let rpcOk = "OK\n"

let rpcNokTag = "NOK "
let rpcErr err = rpcNokTag ^ err ^ "\n"

type handshakeMsg = Ok | Error of string | Unknown of string

let receiveHandshakeMsg conn =
  receiveUntilSpaceOrNl conn >>= fun msg ->
  if msg = rpcOk then Lwt.return Ok
  else if msg = rpcNokTag then begin
    receiveUntilNewlineNb conn >>= fun msg -> Lwt.return (Error msg)
  end else
    Lwt.return (Unknown msg)

type handshakeData = Data of string | Error of string | Unknown of string

let receiveHandshakeData conn keyw =
  receiveUntilSpaceOrNl conn >>= fun msg ->
  if msg = keyw then
    receiveUntilNewline conn >>= fun data -> Lwt.return (Data data)
  else if msg = rpcNokTag then
    receiveUntilNewlineNb conn >>= fun err -> Lwt.return (Error err)
  else
    Lwt.return (Unknown msg)

let sendHandshakeMsg conn = function
  | Ok -> sendString conn rpcOk
  | Error err -> sendString conn (rpcErr err)
  | Unknown _ -> assert false

let sendHandshakeErr conn err =
  sendHandshakeMsg conn (Error err)

let sendHandshakeData conn keyw data =
  let len = String.length keyw in
  let keyw = if len > 0 && keyw.[len - 1] <> ' ' then keyw ^ " " else keyw in
  sendString conn (keyw ^ data ^ "\n")

(* RPC version negotiation process:
   1. Server sends connectionHeader and supported RPC versions.

   2. Client receives and verifies connectionHeader.
      * If OK then proceeds.
      * If NOK then closes connection.

   3. Client receives and verifies RPC versions.
      * If not correct version tag or can't parse then closes connection.

   4. Client selects a version (typically the most recent one) from the
      intersection of its supported RPC versions and server's RPC versions.
      * If intersection is empty then closes connection.

   5. Client sends selected RPC version to the server.

   6. Server receives and verifies proposed version.
      * If OK then proceeds.
      * If not correct version tag, can't parse or proposed version is
        not supported then server sends "NOK".
      ** Client receives "NOK" and closes connection.

   7. Server selects proposed version and sends "OK".

   8. Client receives "OK". Version negotiation is complete.
*)

let connectionHeader = "Unison RPC\n"
let compatConnectionHeader = "Unison 2.51 with OCaml >= 4.01.2\n"
(* Every supported version released prior to the RPC version negotiation
   mechanism uses this connection header string. *)
let compat248ConnectionHeader = "Unison 2.48\n"
(* Additionally, even 2.48 can be supported, even though that support is
   not official. *)

let rpcVersionsTag = "VERSIONS "
let rpcVersionsStr = rpcVersionsTag ^ rpcSupportedVersionStrHdr ^ "\n"

let rpcVersionTag = "VERSION "
let rpcVersionStr ver = rpcVersionTag ^ string_of_int ver ^ "\n"

let verIsSupported ver =
  Safelist.exists (fun v -> v = ver) rpcSupportedVersions

let handshakeFail err =
  Lwt.fail (Util.Fatal err)

let handshakeError msg =
  handshakeFail ("Received error from the server: \"" ^ msg ^ "\".")

let handshakeUnknown msg =
  handshakeFail ("Received unexpected header from the server: \""
                 ^ String.escaped msg ^ "\".")

let parseVersion side s =
  let error e =
    raise (Util.Transient
            ("Unknown " ^ side ^ " RPC version: " ^ e
             ^ ". Version received from " ^ side ^ ": \"" ^ String.escaped s
             ^ "\". Supported RPC versions: " ^ rpcSupportedVersionStr))
  in
  if s = "" then
    error "invalid format"
  else
    match int_of_string s with
    | ver -> Some ver
    | exception Failure _ -> error "parse error"

let parseServerVersions inp =
  let supported l = function
    | "" -> l
    | v -> match parseVersion "server" v with
           | Some vi -> if verIsSupported vi then vi :: l else l
           | None -> l
  in
  try
    let vs = String.split_on_char ' ' inp in
    if vs = [""] then ignore (parseVersion "server" ""); (* Trigger the error *)
    let intersect = Safelist.fold_left supported [] vs in
    Lwt.return (Safelist.rev (Safelist.sort compare intersect))
  with
  | Util.Transient e -> handshakeFail e

let selectServerVersion conn =
  let getTheRest () = Bytes.to_string (peekWithoutBlocking conn.inputBuffer) in
  receiveHandshakeData conn rpcVersionsTag >>= function
  | Error msg -> handshakeError msg
  | Unknown fromServ -> handshakeUnknown (fromServ ^ getTheRest ())
  | Data versions ->
      parseServerVersions versions >>= function
      | [] ->
          handshakeFail ("None of server's RPC versions are supported. "
                         ^ "The server may be too old or too recent. "
                         ^ "Versions received from server: \""
                         ^ String.escaped versions ^ "\". "
                         ^ "Supported RPC versions: " ^ rpcSupportedVersionStr)
      | ver :: _ ->
          setConnectionVersion conn ver;
          debug (fun () -> Util.msg "Selected RPC version: %i\n" ver);
          sendHandshakeData conn rpcVersionTag (string_of_int ver) >>= fun () ->
          receiveHandshakeMsg conn >>= function
          | Ok -> Lwt.return ()
          | Error reply -> handshakeError reply
          | Unknown reply -> handshakeUnknown (reply ^ getTheRest ())

let checkServerVersion conn header =
  if header = compatConnectionHeader then begin
    setConnectionVersion conn 0;
    debug (fun () -> Util.msg "Selected RPC version: 2.51-compatibility\n");
    (* skip negotiation *) Lwt.return ()
  end else if header = compat248ConnectionHeader then begin
    setConnectionVersion conn 0;
    debug (fun () -> Util.msg "Selected RPC version: 2.48-compatibility\n");
    (* skip negotiation *) Lwt.return ()
  end else
    selectServerVersion conn

let rec checkHeaderRec conn buffer pos len connectionHeader =
  if pos = len then
    Lwt.return connectionHeader
  else begin
    (grab conn.inputBuffer buffer 1 >>= (fun () ->
    let chOk =
      try buffer.{0} = connectionHeader.[pos] with Invalid_argument _ -> false
    and compatChOk =
      try buffer.{0} = compatConnectionHeader.[pos] with Invalid_argument _ -> false
    and compat248ChOk =
      try buffer.{0} = compat248ConnectionHeader.[pos] with Invalid_argument _ -> false
    in
    if not chOk && not compatChOk && not compat248ChOk then
      let prefix =
        String.sub connectionHeader 0 pos ^ Bytearray.to_string buffer in
      let rest = peekWithoutBlocking conn.inputBuffer in
      Lwt.fail
        (Util.Fatal
           ("Received unexpected header from the server:\n \
             expected \""
           ^ String.escaped (* (String.sub connectionHeader 0 (pos + 1)) *)
               connectionHeader
           ^ "\" but received \"" ^ String.escaped (prefix ^ Bytes.to_string rest) ^ "\", \n"
           ^ "which differs at \"" ^ String.escaped prefix ^ "\".\n"
           ^ "This can happen because you have different versions of Unison\n"
           ^ "installed on the client and server machines, or because\n"
           ^ "your connection is failing and somebody is printing an error\n"
           ^ "message, or because your remote login shell is printing\n"
           ^ "something itself before starting Unison."))
    else
    if not chOk && compatChOk then
      (* We make use of the fact that that the new header is almost a prefix
         of the old header. It is not an exact comparison here but good
         enough for this purpose. *)
      checkHeaderRec conn buffer (pos + 1)
        (String.length compatConnectionHeader) compatConnectionHeader
    else if not chOk && compat248ChOk then
      checkHeaderRec conn buffer (pos + 1)
        (String.length compat248ConnectionHeader) compat248ConnectionHeader
    else
      checkHeaderRec conn buffer (pos + 1) len connectionHeader))
  end

let checkHeader conn =
  checkHeaderRec conn (Bytearray.create 1) 0
    (String.length connectionHeader) connectionHeader

(****)

(* Magic string exchange is used within the old protocol to detect if both
   the server and the client support the new RPC version negotiation mechanism.

   It works like this:
    1. Directly after connection header, the server sends the magic string and
       otherwise continues using the old RPC protocol.
    2. An old client will process the magic string as a valid RPC message that
       is effectively a no-op and continues using old RPC protocol as normal.
    3. A new client will notice the magic string and send the same magic string
       in response. It will stop the old RPC protocol and restart from header
       checking and what is now hopefully an RPC version negotiation.
    4. The server will notice client's magic string, stop the old RPC protocol
       and restart from connection header, this time with the new RPC version
       negotiation mechanism.

   The magic string is defined as follows:
    1. encoded int 1 followed by
    2. encoded int > 0 (packet size) followed by
    3. a valid 2.51 protocol packet, the contents of which we don't care about,
       but it must be a no-op for 2.51 client (in this case a StreamAbort).

   Int 1 is a valid 2.51 protocol message ID but it is never used with normal
   messages, hence its safe usage as a magic string. A StreamAbort to a client,
   especially with id 1, is a safe no-op. *)

let magicId = 1
(* Although this magic packet is inherently dependent on OCaml version,
   it is unlikely to change and has been verified to be the same with
   OCaml versions 4.05 to 4.12. It is hard coded here to avoid any future
   changes (the idea being that old clients will not be compiled with
   any newer OCaml compilers). *)
let magicPacket = "rsp\132\149\166\190\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000A"
let magic = encodeInt magicId :: encodeInt (String.length magicPacket) ::
              [Bytearray.of_string magicPacket, 0, String.length magicPacket]

let checkForMagicString conn =
  (* Fill the buffer and then peek at the contents without consuming *)
  peekWithBlocking conn.inputBuffer >>= fun b ->
  if Bytes.length b < intSize then
    Lwt.return false
  else begin
    let id = Bytearray.create intSize in
    let () = Bytearray.blit_from_bytes b 0 id 0 intSize in
    if decodeInt id 0 <> magicId then
      Lwt.return false
    else begin
      debug (fun () -> Util.msg "Received RPC version upgrade notice\n");
      (* Consume magic id from buffer *)
      grab conn.inputBuffer id intSize >>= fun () ->
      (* Consume magic packet from buffer *)
      receivePacket conn >>= fun _ -> Lwt.return true
      (* We rely solely on the magic id and don't check the contents of the
         packet. Should it become necessary for some reason then it is
         possible to verify the magic packet byte by byte here. *)
    end
  end

let checkServerUpgrade conn header =
  if header <> compatConnectionHeader && header <> compat248ConnectionHeader then
    Lwt.return header
  else
    checkForMagicString conn >>= function
    | false -> Lwt.return header
    | true ->
        (* Consume write token from buffer *)
        let id = Bytearray.create intSize in
        grab conn.inputBuffer id intSize >>= fun () ->
        (* Send the magic string *)
        dumpUrgent conn magic >>= fun () ->
        debug (fun () -> Util.msg "Going to attempt RPC version upgrade\n");
        checkHeader conn

(****)

(*
   Disable flow control if possible.
   Both hosts must use non-blocking I/O (otherwise a dead-lock is
   possible with ssh).
*)
let halfduplex =
  Prefs.createBool "halfduplex" false
    ~category:(`Advanced `Remote)
    ~deprecated:true
    "force half-duplex communication with the server"
    "When this flag is set to {\\tt true}, Unison network communication \
     is forced to be half duplex (the client and the server never \
     simultaneously emit data).  If you experience unstabilities with \
     your network link, this may help."

let negociateFlowControlLocal conn () =
  disableFlowControl conn.outputQueue;
  Lwt.return false

let negociateFlowControlRemote =
  registerServerCmd "negociateFlowControl" Umarshal.unit Umarshal.bool negociateFlowControlLocal

let negociateFlowControl conn =
  (* Flow control negotiation can be done asynchronously. *)
  if not (Prefs.read halfduplex) then
    Lwt.ignore_result
      (negociateFlowControlRemote conn () >>= fun needed ->
       if not needed then
         negociateFlowControlLocal conn ()
       else
         Lwt.return true)

(****)

let initConnection ?(connReady=fun () -> ()) ?cleanup in_ch out_ch =
  (* [makeConnection] is not expected to raise any recoverable exceptions.
     If this assumption changes in the future then [in_ch] and [out_ch] must
     be closed in the recovery code. *)
  let conn = makeConnection false in_ch out_ch in
  let close_on_fail t =
    Lwt.catch (fun () -> t) (fun e -> closeConnection conn; Lwt.fail e)
  in
  let with_timeout t =
    Lwt.choose [t;
      Lwt_unix.sleep 120. >>= fun () ->
      Lwt.fail (Util.Fatal "Timed out negotiating connection with the server")]
  in
  close_on_fail (with_timeout (
    peekWithBlocking conn.inputBuffer >>= fun _ ->
    connReady (); Lwt.return () >>= fun () -> (* Connection working, notify *)
    checkHeader conn >>=
    checkServerUpgrade conn >>=
    checkServerVersion conn)) >>= fun () ->
  registerConnCleanup conn cleanup;
  (* From this moment forward, the RPC version has been selected. All
     communication must now adhere to that version's specification. *)
  enableFlowControl conn false >>= (fun () ->
  Lwt.ignore_result (Lwt.catch
    (fun () -> receive conn)
    (fun e ->
      clientConnClose conn;
      if isConnectionCheck conn then Lwt.return () else Lwt.fail e));
  negociateFlowControl conn;
  Lwt.return conn)

let rec findFirst f l =
  match l with
    []     -> Lwt.return None
  | x :: r -> f x >>= fun v ->
              match v with
                None        -> findFirst f r
              | Some _ as v -> Lwt.return v

let printAddr host addr =
  match addr with
    Unix.ADDR_UNIX s ->
      s
  | Unix.ADDR_INET (s, p) ->
      Format.sprintf "%s[%s]:%d" host (Unix.string_of_inet_addr s) p

let buildSocket host port kind ?(err="") ai =
  let attemptCreation ai =
    Lwt.catch
      (fun () ->
         let socket =
           Lwt_unix.socket ~cloexec:true
             ai.Unix.ai_family ai.Unix.ai_socktype ai.Unix.ai_protocol
         in
         Lwt.catch
           (fun () ->
              begin match kind with
                `Connect ->
                  (* Connect (synchronously) to the remote host *)
                  Lwt_unix.connect socket ai.Unix.ai_addr
              | `Bind ->
                  (* Some OS (Linux?) enable dual-stack mode by default;
                     trying to bind both IPv4 and IPv6 sockets will fail
                     with EADDRINUSE unless dual-stack mode is disabled. *)
                  if ai.Unix.ai_family = Unix.PF_INET6 then
                    Lwt_unix.setsockopt socket Unix.IPV6_ONLY true;
                  (* Allow reuse of local addresses for bind *)
                  if ai.Unix.ai_family <> Unix.PF_UNIX then
                    Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
                  (* Bind the socket to portnum on the local host
                     or to a filesystem path (when Unix domain socket) *)
                  Lwt_unix.bind socket ai.Unix.ai_addr;
                  (* Start listening, allow up to 1 pending request *)
                  Lwt_unix.listen socket 1;
                  Lwt.return ()
              end >>= fun () ->
              Lwt.return (Some socket))
           (fun e ->
              match e with
                Unix.Unix_error _ ->
                  Lwt_unix.close socket;
                  Lwt.fail e
              | _ ->
                  Lwt.fail e))
      (fun e ->
         match e with
           Unix.Unix_error (error, _, _) ->
             begin match error with
               Unix.EAFNOSUPPORT | Unix.EPROTONOSUPPORT | Unix.EINVAL ->
                 Lwt.return None
             | _  ->
                 let msg =
                   match kind with
                     `Connect ->
                       Printf.sprintf "%s%s: %s\n"
                         err
                         (printAddr host ai.Unix.ai_addr)
                         (Unix.error_message error)
                   | `Bind when ai.Unix.ai_family <> Unix.PF_UNIX ->
                       Printf.sprintf
                         "Can't bind socket to port %s at address [%s]: %s\n"
                         port
                         (match ai.Unix.ai_addr with
                            Unix.ADDR_INET (addr, _) ->
                              Unix.string_of_inet_addr addr
                          | _ ->
                              assert false)
                         (Unix.error_message error)
                   | `Bind (* Unix.PF_UNIX *) ->
                       Printf.sprintf
                         "Can't bind socket to path '%s': %s\n"
                         port
                         (Unix.error_message error)
                 in
                 Lwt.fail (Util.Fatal msg)
             end
         | _ ->
             Lwt.fail e)
  in
  attemptCreation ai

let makeUnixSocketAi path =
  { Unix.ai_family = Unix.PF_UNIX;
    ai_socktype = Unix.SOCK_STREAM;
    ai_protocol = 0;
    ai_addr = Unix.ADDR_UNIX path;
    ai_canonname = "" }

let buildConnectSocketUnix path =
  assert (String.length path > 2);
  (* Unix domain socket path from [Clroot] is enclosed in curly braces.
     Extract the real path. *)
  let path = String.sub path 1 ((String.length path) - 2) in
  let err = "Can't connect to Unix domain socket on path " in
  buildSocket "" path `Connect ~err (makeUnixSocketAi path) >>= function
  | None ->
      Lwt.fail (Util.Fatal (err ^ path))
  | Some x ->
      Lwt.return x

let buildConnectSocket host port =
  let isHost = String.length host > 0 && host.[0] <> '{' in
  if not isHost then buildConnectSocketUnix host else
  let err = "Failed to connect to the server on host " in
  let attemptCreation ai = buildSocket host port `Connect ~err ai in
  let options = [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ] in
  findFirst attemptCreation (Unix.getaddrinfo host port options) >>= fun res ->
  match res with
    Some socket ->
      Lwt.return socket
  | None ->
      let hostport = Printf.sprintf "%s:%s" host port in
      Lwt.fail (Util.Fatal (err ^ hostport))

(* [at_exit] does not provide reliable cleanup (why?), so this
   complex mechanism is needed to unlink Unix domain sockets
   in case of exceptional termination. *)
let createdUnixSockets = ref []

let postponeUnixSocketCleanup path =
  createdUnixSockets := path :: !createdUnixSockets

let unixSocketCleanup () =
  Safelist.iter
    (fun path -> try Unix.unlink path with Unix.Unix_error _ -> ())
    !createdUnixSockets

let buildListenSocketUnix path =
  assert (path <> "");
  buildSocket "" path `Bind (makeUnixSocketAi path) >>= function
  | None ->
      Lwt.fail (Util.Fatal
        (Printf.sprintf "Can't bind Unix domain socket on path %s" path))
  | Some x ->
      postponeUnixSocketCleanup path;
      Lwt.return [x]

let buildListenSocket hosts port =
  let isPort = try ignore (int_of_string port); true with Failure _ -> false in
  if not isPort then buildListenSocketUnix port else
  let options = [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ; Unix.AI_PASSIVE ] in
  hosts
  |> Safelist.map (fun host -> Unix.getaddrinfo host port options)
  |> Safelist.concat
  |> Lwt_util.map (buildSocket "" port `Bind) >>= fun res ->
  match Safelist.filter (fun x -> x <> None) res with
  | [] ->
      Lwt.fail (Util.Fatal (Printf.sprintf "Can't bind socket to port %s" port))
  | s ->
      Lwt.return (Safelist.map (function None -> assert false | Some x -> x) s)

let buildSocketConnection host port =
  buildConnectSocket host port >>= fun socket ->
  initConnection socket socket

let buildShellConnection shell host userOpt portOpt rootName termInteract =
  let remoteCmd =
    (if Prefs.read serverCmd="" then Uutil.myName
     else Prefs.read serverCmd)
    ^ (if Prefs.read addversionno then "-" ^ Uutil.myMajorVersion else "")
    ^ " -server " ^ rpcServerCmdlineOverride in
  let userArgs =
    match userOpt with
      None -> []
    | Some user -> ["-l"; user] in
  let portArgs =
    match portOpt with
      None -> []
    | Some port -> ["-p"; port] in
  let shellCmd =
    (if shell = "ssh" then
      Prefs.read sshCmd
    else
      shell) in
  let shellCmdArgs =
    (if shell = "ssh" then
      Prefs.read sshargs
    else
      "") in
  let preargs =
    (userArgs @ portArgs @
       [host]@
       (if shell="ssh" then ["-e none"] else [])@
       [shellCmdArgs;remoteCmd]) in
  (* Split compound arguments at space chars, to make
     create_process happy *)
  let args = [shellCmd] @
    Safelist.concat
      (Safelist.map (fun s -> Util.splitIntoWords s ' ') preargs) in
  let argsarray = Array.of_list args in
  let (i1,o1) = Lwt_unix.pipe_out ~cloexec:true () in
  let (i2,o2) = Lwt_unix.pipe_in ~cloexec:true () in
  (* We need to make sure that there is only one reader and one
     writer by pipe, so that, when one side of the connection
     dies, the other side receives an EOF or a SIGPIPE. *)
  debug (fun ()-> Util.msg "Shell connection: %s (%s)\n"
           shellCmd (String.concat ", " args));
  let (term, termPid) =
    Util.convertUnixErrorsToFatal "starting shell connection" (fun () ->
    match termInteract with
    | None ->
        (* Signals generated by the terminal from user input are sent to all
           processes in the foreground process group. This means that the ssh
           child process will receive SIGINT at the same time as Unison and
           close the connection before Unison has the chance to do cleanup with
           the remote end. To make matters more complicated, the ssh process
           must be in the foreground process group because interaction with the
           user is done via the terminal (not via stdin, stdout) and background
           processes can't read from the terminal (unless we'd set up a pty
           like is done for the GUI).

           Don't let these signals reach ssh by blocking them.

           Unfortunately, a bug introduced in OpenSSH 9.6 (also present in 9.7)
           breaks this workaround by unblocking SIGINT in the ssh process.

           The signals could be ignored instead of being blocked because ssh
           does not set handlers for SIGINT and SIGQUIT if they've been ignored
           at startup. But this triggers an error in ssh. The interactive
           passphrase reading function captures these signals for the purpose
           of restoring terminal settings (echo). When receiving a signal, and
           after restoring previous signal handlers, it resends the signal to
           itself. But now the signal is ignored and instead of terminating,
           the process will continue running as if passphrase reading function
           had returned with an empty result.

           Since the ssh process no longer receives the signals generated by
           user input we have to make sure that it terminates when Unison does.
           This usually happens due to its stdin and stdout being closed,
           except for when it is interacting with the user via terminal. To get
           around that, an [at_exit] handler is registered to send a SIGTERM
           and SIGKILL to the ssh process.  (Note, for [at_exit] handlers to
           run, unison process must terminate normally, not be killed. For
           SIGINT, this means that [Sys.catch_break true] (or an alternative
           SIGINT handler) must be set before creating the ssh process.) *)
        let pid = Util.blockSignals [Sys.sigint] (fun () ->
          System.create_process shellCmd argsarray i1 o2 Unix.stderr) in
        let end_ssh () =
          let kill_noerr si = try Unix.kill pid si
            with Unix.Unix_error _ -> () | Invalid_argument _ -> () in
          match Unix.waitpid [WNOHANG] pid with
          | (0, _) ->
              (* Grace period before killing. Important to give ssh a chance
                 to restore terminal settings, should that be needed. *)
              kill_noerr Sys.sigterm; Unix.sleepf 0.01; kill_noerr Sys.sigkill
          | _ | exception Unix.Unix_error _ -> ()
        in
        let () = at_exit end_ssh in
        (None, pid)
    | Some callBack ->
        Terminal.create_session shellCmd argsarray i1 o2 Unix.stderr)
  in
  Unix.close i1; Unix.close o2;
  let writeSilentNoexn fd s pos len =
    ignore (try if len > 0 then Unix.write fd s pos len else 0
            with Unix.Unix_error _ -> 0)
  in
  let forwardShellStderr fdIn fdOut s =
    (* When the shell connection has been established then keep
       forwarding server's stderr to client's stderr; not to GUI. *)
    let buf = Bytes.create 16000 in
    let rec loop s len =
      (* Can't use printf because if stderr is not open in Windows,
         it will throw an exception when at_exit tries to flush it. *)
      writeSilentNoexn fdOut s 0 len;
      Lwt.catch (fun () -> Lwt_unix.read fdIn buf 0 16000)
        (fun _ -> debug (fun () ->
           Util.msg "Caught an exception when reading remote stderr\n");
           Lwt.return 0)
      >>= function
      | 0 -> Lwt.return ()
      | len -> loop buf len
    in
    loop (Bytes.of_string s) (String.length s)
  in
  let (connReady, getTermErr) =
    match term, termInteract with
    | Some fdTerm, Some interact ->
        let (setReady, handleRequests, extractRemainingOutput) =
          Terminal.handlePasswordRequests fdTerm (interact rootName) in
        Lwt.ignore_result (
          handleRequests >>=
          forwardShellStderr (fst fdTerm) Unix.stderr);
        let connReady () =
          let s = setReady () in
          writeSilentNoexn Unix.stderr (Bytes.of_string s) 0 (String.length s)
        in
        (connReady, extractRemainingOutput)
    | _ ->
        (Fun.id, fun () -> Lwt.return "")
  in
  let cleanup () =
    (* Make sure the [handlePasswordRequests] threads will finish while
       silencing any exceptions (most likely EBADF) caused by having closed
       the terminal fds. *)
    Lwt.ignore_result (getTermErr () >>= fun s ->
      debug (fun () ->
        if s <> "" then Util.msg "Received from remote shell process:\n%s\n" s);
      Lwt.return ());
    if term = None then
      try ignore (Terminal.safe_waitpid termPid) with Unix.Unix_error _ -> ()
    else
      try Terminal.close_session termPid with Unix.Unix_error _ -> ()
  in
  (* With [connReady], we know that shell connection was established (even if
     RPC handshake failed). This hacky way of detecting the connection is used
     because [Lwt_unix.wait_read] is not implemented under Windows.
     By this time, we are already somewhat late in the communication process.
     Any error output from very early stages of server startup, before other
     output is produced, might still end up in GUI (but this is very unlikely;
     it is more likely that the same error caused connection to be dropped). *)
  Lwt.catch
    (fun () -> initConnection ~connReady ~cleanup i2 o1)
    (fun e ->
      Lwt.catch
        (fun () -> getTermErr () >>= fun s ->
                   if s <> "" then Util.warn s;
                   Lwt.fail e)
        (* Don't close the terminal before reading the final error output
           or we might miss it completely. *)
        (fun _ ->  cleanup (); Lwt.fail e))

let canonizeLocally s =
  Fspath.canonize s

let canonizeOnServer =
  registerServerCmd "canonizeOnServer"
    Umarshal.(prod2 (option string) bool id id)
    Umarshal.(prod2 string Fspath.m id id)
    (fun _ (s, _) -> (* The tuple is kept for backwards API compatibility *)
       Lwt.return (Os.myCanonicalHostName (), canonizeLocally s))

let canonizeOnServer conn s =
  (* The second tuple item is required for compatibility with <= 2.52 *)
  canonizeOnServer conn (s, true)

let canonize clroot = (* connection for clroot must have been set up already *)
  match clroot with
    Clroot.ConnectLocal s ->
      (Common.Local, canonizeLocally s)
  | _ ->
      match ClientConn.canonRootOfClroot clroot with
        None                -> raise (Util.Fatal "Remote.canonize")
      | Some root -> root

let isRootConnected = function
  | (Common.Local, _) -> true
  | (Common.Remote _, _) as root -> ClientConn.ofRootConncheck root <> None

let canonizeRoot rootName clroot termInteract =
  let finish ioServer s =
    (* We need to always compute the fspath as it may have changed
       due to profile configuration changes *)
    canonizeOnServer ioServer s >>= (fun (host, fspath) ->
    let root = (Common.Remote host, fspath) in
    ClientConn.register clroot root ioServer;
    Lwt.return root) in
  match clroot with
    Clroot.ConnectLocal s ->
      Lwt.return (Common.Local, canonizeLocally s)
  | Clroot.ConnectBySocket(host,port,s) ->
      begin match ClientConn.ofClrootConncheck clroot with
      | Some x -> Lwt.return x
      | None   -> buildSocketConnection host port
      end >>= fun ioServer ->
      finish ioServer s
  | Clroot.ConnectByShell(shell,host,userOpt,portOpt,s) ->
      begin match ClientConn.ofClrootConncheck clroot with
      | Some x -> Lwt.return x
      | None   -> buildShellConnection
                   shell host userOpt portOpt rootName termInteract
      end >>= fun ioServer ->
      finish ioServer s

(* A new interface, useful for terminal interaction, it should
   eventually replace canonizeRoot and buildShellConnection *)
(* A preconnection is None if there's nothing more to do, and Some if
   terminal interaction might be required (for ssh password) *)
type preconnection =
     (Unix.file_descr
     * Lwt_unix.file_descr
     * Lwt_unix.file_descr
     * Unix.file_descr
     * string option
     * (Lwt_unix.file_descr * Lwt_unix.file_descr) option
     * Clroot.clroot
     * int)
let openConnectionStart clroot =
  match clroot with
    Clroot.ConnectLocal s ->
      None
  | Clroot.ConnectBySocket(host,port,s) ->
      Lwt_unix.run
        (begin match ClientConn.ofClrootConncheck clroot with
         | Some x -> Lwt.return x
         | None   -> buildSocketConnection host port
         end >>= fun ioServer ->
         (* We need to always compute the fspath as it may have changed
            due to profile configuration changes *)
         canonizeOnServer ioServer s >>= fun (host, fspath) ->
         ClientConn.register clroot (Common.Remote host, fspath) ioServer;
         Lwt.return ());
      None
  | Clroot.ConnectByShell(shell,host,userOpt,portOpt,s) ->
      match ClientConn.ofClrootConncheck clroot with
      | Some ioServer ->
           (* We recompute the fspath as it may have changed due to
              profile configuration changes *)
           Lwt_unix.run
             (canonizeOnServer ioServer s >>= fun (host, fspath) ->
              ClientConn.register clroot (Common.Remote host, fspath) ioServer;
              Lwt.return ());
           None
      | None ->
          let remoteCmd =
            (if Prefs.read serverCmd="" then Uutil.myName
             else Prefs.read serverCmd)
            ^ (if Prefs.read addversionno then "-" ^ Uutil.myMajorVersion else "")
            ^ " -server " ^ rpcServerCmdlineOverride in
          let userArgs =
            match userOpt with
              None -> []
            | Some user -> ["-l"; user] in
          let portArgs =
            match portOpt with
              None -> []
            | Some port -> ["-p"; port] in
          let shellCmd =
            (if shell = "ssh" then
              Prefs.read sshCmd
            else
              shell) in
          let shellCmdArgs =
            (if shell = "ssh" then
              Prefs.read sshargs
            else
              "") in
          let preargs =
              ([shellCmd]@userArgs@portArgs@
               [host]@
               (if shell="ssh" then ["-e none"] else [])@
               [shellCmdArgs;remoteCmd]) in
          (* Split compound arguments at space chars, to make
             create_process happy *)
          let args =
            Safelist.concat
              (Safelist.map (fun s -> Util.splitIntoWords s ' ') preargs) in
          let argsarray = Array.of_list args in
          let (i1,o1) = Lwt_unix.pipe_out ~cloexec:true () in
          let (i2,o2) = Lwt_unix.pipe_in ~cloexec:true () in
          (* We need to make sure that there is only one reader and one
             writer by pipe, so that, when one side of the connection
             dies, the other side receives an EOF or a SIGPIPE. *)
          debug (fun ()-> Util.msg "Shell connection: %s (%s)\n"
                   shellCmd (String.concat ", " args));
          let (term,pid) =
            Terminal.create_session shellCmd argsarray i1 o2 Unix.stderr in
          (* after terminal interact, remember to close i1 and o2 *)
          Some(i1,i2,o1,o2,s,term,clroot,pid)

let openConnectionPrompt = function
    (i1,i2,o1,o2,s,Some fdTerm,clroot,pid) ->
      let x = Terminal.termInput fdTerm i2 in
      x
  | _ -> None

let openConnectionReply = function
    (i1,i2,o1,o2,s,Some fdTerm,clroot,pid) ->
    (fun response ->
      (* FIX: should loop until everything is written... *)
      ignore (Lwt_unix.run (Lwt_unix.write (snd fdTerm) (Bytes.of_string (response ^ "\n")) 0
                              (String.length response + 1))))
  | _ -> (fun _ -> ())

let openConnectionEnd (i1,i2,o1,o2,s,fdopt,clroot,pid) =
      Unix.close i1; Unix.close o2;
      let cleanup () =
        try Terminal.close_session pid with Unix.Unix_error _ -> ()
      in
      Lwt_unix.run
        (initConnection ~cleanup i2 o1 >>= fun ioServer ->
         canonizeOnServer ioServer s >>= fun (host, fspath) ->
         ClientConn.register clroot (Common.Remote host, fspath) ioServer;
         Lwt.return ())

let openConnectionCancel (i1,i2,o1,o2,s,fdopt,clroot,pid) =
  (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
  (try Unix.close i1 with Unix.Unix_error _ -> ());
  (try Lwt_unix.close i2 with Unix.Unix_error _ -> ());
  (try Lwt_unix.close o1 with Unix.Unix_error _ -> ());
  (try Unix.close o2 with Unix.Unix_error _ -> ());
  (try Terminal.close_session pid with Unix.Unix_error _ -> ())

(****************************************************************************)
(*                     SERVER-MODE COMMAND PROCESSING LOOP                  *)
(****************************************************************************)

let checkClientVersion conn () =
  let reply msg = sendHandshakeMsg conn msg in
  (* FIX: In future when gaining the ability to close connections from server
     side, make errors close the connection, not just send to client. *)
  let error = sendHandshakeErr conn in
  receiveHandshakeData conn rpcVersionTag >>= function
  | Error msg ->
      error ("Could not negotiate RPC version. "
             ^ "Received unexpected error from the client: \"" ^ msg ^ "\"")
  | Unknown fromClient ->
      error ("Could not negotiate RPC version. "
             ^ "Received unexpected header from the client: \""
             ^ String.escaped (fromClient
             ^ Bytes.to_string (peekWithoutBlocking conn.inputBuffer)) ^ "\"")
  | Data buf ->
      match parseVersion "client" buf with
      | Some clientVer ->
          if verIsSupported clientVer then begin
            setConnectionVersion conn clientVer;
            reply Ok
          end else
            error ("Client RPC version not supported. "
                   ^ "Version received from client: \""
                   ^ string_of_int clientVer ^ "\". "
                   ^ "Supported RPC versions: " ^ rpcSupportedVersionStr)
      | None -> Lwt.return ()
      | exception Util.Transient e -> error e

(****)

let showWarningOnClient =
    (registerServerCmd
       "showWarningOnClient" Umarshal.string Umarshal.unit
       (fun _ str -> Lwt.return (Util.warn str)))

let forwardMsgToClient =
    (registerServerCmd
       "forwardMsgToClient" Trace.mmsg Umarshal.unit
       (fun _ str -> (*msg "forwardMsgToClient: %s\n" str; *)
          Lwt.return (Trace.displayMessageLocally str)))

(* Compatibility mode for 2.51 clients. *)
let compatServerInit mode conn =
  let compatConnectionHeader =
    match mode with
    | Some "2.48" -> compat248ConnectionHeader
    | _ -> compatConnectionHeader
  in
  dump conn [(Bytearray.of_string compatConnectionHeader, 0,
                String.length compatConnectionHeader)] >>= fun () ->
  (* Send the magic string to notify new clients *)
  dumpUrgent conn magic >>= fun () ->
  (* Must enable flow control because that is the default for 2.51.
     This must be done after dumpUrgent above to ensure that the write
     token is sent the last. *)
  enableFlowControl conn true >>= fun () ->
  (* Let's see if the client noticed the magic string. This is
     a no-op for old clients. *)
  checkForMagicString conn

let compatServerRun conn =
  (* Set the local warning printer to make an RPC to the client and
     show the warning there; ditto for the message printer *)
  Util.warnPrinter :=
    Some (fun str -> Lwt_unix.run (showWarningOnClient conn str));
  Trace.messageForwarder :=
    Some (fun str -> Lwt_unix.run (forwardMsgToClient conn str));
  receive conn >>=
  Lwt.wait

(* This function loops, waits for commands, and passes them to
   the relevant functions. *)
let commandLoop ~compatMode in_ch out_ch =
  Trace.runningasserver := true;
  (* Send header indicating to the client that it has successfully
     connected to the server *)
  let conn = makeConnection true in_ch out_ch in
  Lwt.catch
    (fun () ->
       (if compatMode <> None then
         let () = setConnectionVersion conn 0 in
         compatServerInit compatMode conn >>= (fun upgrade ->
         if upgrade then begin
           (* Restore the state before starting protocol negotiation *)
           allowWrites conn.outputQueue;
           disableFlowControl conn.outputQueue
         end;
         Lwt.return upgrade)
       else
         Lwt.return true) >>= fun upgrade ->
       debug (fun () -> Util.msg "%sGoing to attempt RPC version upgrade\n"
                 (if upgrade then "" else "NOT "));
       if not upgrade then
         compatServerRun conn
       else
       sendStrings conn [connectionHeader; rpcVersionsStr] >>=
       checkClientVersion conn >>= fun () ->
       (* From this moment forward, the RPC version has been selected. All
          communication must now adhere to that version's specification. *)
       (* Flow control was disabled for RPC version handshake. Enable it
          for flow control negotiation. *)
       enableFlowControl conn true >>= (fun () ->
       (* Set the local warning printer to make an RPC to the client and
          show the warning there; ditto for the message printer *)
       Util.warnPrinter :=
         Some (fun str -> Lwt_unix.run (showWarningOnClient conn str));
       Trace.messageForwarder :=
         Some (fun str -> Lwt_unix.run (forwardMsgToClient conn str));
       receive conn >>=
       Lwt.wait))
    (fun e ->
       match e with
         Util.Fatal "Lost connection with the server" ->
           debug (fun () -> Util.msg "Connection closed by the client\n");
           (* We prevent new writes and wait for any current write to
              terminate.  As we don't have a good way to wait for the
              writer to terminate, we just yield a bit. *)
           let rec wait n =
             if n = 0 then Lwt.return () else begin
               Lwt_unix.yield () >>= fun () ->
               wait (n - 1)
             end
           in
           conn.outputBuffer.opened <- false;
           wait 10
       | _ ->
           Lwt.fail e)

let killServer =
  Prefs.createBool "killserver" false
    ~category:(`Advanced `Remote)
    "kill server when done (even when using sockets)"
    ("When set to \\verb|true|, this flag causes Unison to kill the remote "
     ^ "server process when the synchronization is finished.  This behavior "
     ^ "is the default for \\verb|ssh| connections, so this preference is not "
     ^ "normally needed when running over \\verb|ssh|; it is provided so "
     ^ "that socket-mode servers can be killed off after a single run of "
     ^ "Unison, rather than waiting to accept future connections.  (Some "
     ^ "users prefer to start a remote socket server for each run of Unison, "
     ^ "rather than leaving one running all the time.)")

(* For backward compatibility *)
let _ = Prefs.alias killServer "killServer"

(* FIX: This code should be removed when removing 2.51-compatibility code. *)
let is248Exe =
  let exeName = Filename.basename (Sys.executable_name) in
  String.length exeName >= 11 && String.sub exeName 0 11 = "unison-2.48"

let rec accept_retry l =
  Lwt.catch
    (fun () -> Lwt_unix.accept l)
    (function
     (* Temporary and connection-specific errors *)
     | Unix.Unix_error (Unix.ECONNABORTED, _, _)
     | Unix.Unix_error (Unix.EPERM, _, _)  (* Linux firewall *)
     | Unix.Unix_error
         (* Resource exhaustion: could be considered temporary *)
         (Unix.(EMFILE | ENFILE | ENOBUFS | ENOMEM), _, _)
       (* Linux curiosity: accept(2) may return errors on the new socket *)
     | Unix.Unix_error (Unix.ENETUNREACH, _, _)
     | Unix.Unix_error (Unix.EHOSTUNREACH, _, _)
     | Unix.Unix_error (Unix.ENETDOWN, _, _)
     | Unix.Unix_error (Unix.EHOSTDOWN, _, _)
     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) as e ->
         let errmsg = match e with
           | Unix.Unix_error (err, _, _) -> Unix.error_message err
           | _ -> Printexc.to_string e in
         Util.msg "server: continuing after receiving an error \
           when accepting client connection: %s\n" errmsg;
         accept_retry l
     (* Permanent errors *)
     | e -> Lwt.fail e)

(* Used by the socket mechanism: Create a socket on portNum and wait
   for a request. Each request is processed by commandLoop. When a
   session finishes, the server waits for another request. *)
let waitOnPort hosts port =
  Util.convertUnixErrorsToFatal "waiting on port"
    (fun () ->
       let hosts = match hosts with [] -> [""] | _ -> hosts in
       let listening = Lwt_unix.run (buildListenSocket hosts port) in
       let accepting = Array.make (Safelist.length listening) None in
       let rec accept i l =
         match accepting.(i) with
           | None ->
               let st = accept_retry l >>= fun s -> Lwt.return (i, s) in
               let () = accepting.(i) <- Some st in
               st
           | Some st -> st
       and serve (i, s) = accepting.(i) <- None; setKeepalive s; s
       and setKeepalive = function
         | (_, Unix.ADDR_UNIX _) -> ()
         | (c, ADDR_INET _) -> Lwt_unix.setsockopt c Unix.SO_KEEPALIVE true
       in
       Util.msg "server started\n";
       let rec handleClients () =
         let (connected, _) =
           serve @@ Lwt_unix.run (Lwt.choose (List.mapi accept listening))
         in
         registerIOClose (connected, connected) (fun () -> doCleanup connected);
         begin try
           (* Accept a connection *)
           let compatMode = Some (if is248Exe then "2.48" else "2.51") in
           Lwt_unix.run (commandLoop ~compatMode connected connected)
         with Util.Fatal "Lost connection with the server" -> () end;
         (* The client has closed its end of the connection *)
         if not (Prefs.read killServer) then handleClients ()
       and doCleanup socket =
         begin try Lwt_unix.close socket with Unix.Unix_error _ -> () end;
         if not (Prefs.read killServer) then runConnCloseHandlers true
       in
       try
         Sys.catch_break true;
         handleClients ();
         unixSocketCleanup ()
       with
       | Sys.Break ->
           unixSocketCleanup ()
       | (Util.Fatal _ | Unix.Unix_error _) as e ->
           unixSocketCleanup ();
           raise e
    )

let beAServer () =
  begin try
    let home = System.getenv "HOME" in
    Util.convertUnixErrorsToFatal
      "changing working directory"
      (fun () -> System.chdir home)
  with Not_found ->
    Util.msg
      "Environment variable HOME unbound: \
       executing server in current directory\n"
  end;
  (* Let's start with 2.51-compatibility mode. Newer clients will add
     a special override keyword in server args that will disable the
     compatibility mode.

     FIX: It is a bit of a hack, so better not make it permanent.
     It was added in 2021 and should be removed after a couple of years. *)
  let compatMode =
     try
       not (Prefs.scanCmdLine "" |> Util.StringMap.find "rest"
            |> Safelist.mem rpcServerCmdlineOverride)
     with Not_found -> true
  in
  (* Additionally, do a best effort emulation of 2.48.
     FIX: remove together with code above. *)
  let compatMode =
    match compatMode with
    | true when is248Exe -> Some "2.48"
    | true -> Some "2.51"
    | false -> None
  in
  begin end;
  Lwt_unix.run
    (commandLoop ~compatMode
       (Lwt_unix.of_unix_file_descr Unix.stdin)
       (Lwt_unix.of_unix_file_descr Unix.stdout))
